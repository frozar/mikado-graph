(ns simulation.core
  (:require
   [bubble.camera :as camera]
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.geometry :as geometry]
   [bubble.state-read :as state-read]
   [cljs.core.async :refer [put!]]
   [cljsjs.d3]
   [clojure.walk :as walk]
   ["/d3/gravity" :as gravity]
   ))

(def current-simulation (atom nil))
(def previous-nodes (atom nil))
(def is-running? (atom false))
(def nodes-geometry (atom nil))

(defn- js-node->cljs-node [nodes]
  (-> nodes
      js->clj
      walk/keywordize-keys))

(defn update-app-state-bubble-position [event-queue]
  (when (and (not (nil? @current-simulation))
           @is-running?)
    (let [nodes (js-node->cljs-node (.nodes @current-simulation))]
      (put! event-queue [:simulation-move nodes]))))

(def square-dist-memoized (memoize geometry/square-dist))

(defn- graph-distance
  "Return the sum of the square distances between each node of two graph."
  [previous-nodes nodes]
  (->>
   (map square-dist-memoized previous-nodes nodes)
   (apply +)))

(defn- graph-converged?
  "Return true if the distance between two graph is smaller than a given 'threshold'."
  [threshold previous-nodes nodes]
  (let [dist (graph-distance previous-nodes nodes)]
    (< dist threshold)))

(defn- ticked [event-queue sim bubble-nodes link-nodes clj-graph]
  (fn tick []
    (let [;; svg-node
          ;; (-> js/d3
          ;;     (.select "#app svg"))
          ;; bubble-nodes
          ;; (-> svg-node
          ;;     (.select "#bubbles")
          ;;     (.selectAll ".bubble"))
          ;; link-nodes
          ;; (-> svg-node
          ;;     (.select "#links")
          ;;     (.selectAll ".link"))
          ;; _ (js/console.log "sim link-nodes" link-nodes)
          ;; bubble-nodes nodes
          ;; link-nodes links
          computed-nodes (js-node->cljs-node (.nodes sim))
          computed-links (-> sim (.force "link") (.links))
          ;; _ (js/console.log "nodes " nodes)
          ;; _ (js/console.log "sim computed-links" computed-links)
          ]
      (.attr bubble-nodes "transform"
             (fn [_ i]
               (when (< i (-> computed-nodes keys count))
                 ;; (js/console.log "tick d " d)
                 ;; (.log js/console "tick i " i)
                 ;; (.log js/console "tick nodes " nodes)
                 ;; (.log js/console "tick (.nodes sim) " (.nodes sim))
                 (let [node (aget (.nodes sim) i)
                       translation-x (.-x node)
                       translation-y (.-y node)]
                   (str "translate(" translation-x " " translation-y ")")))))

      ;; (js/console.log "(.nodes sim) " (.nodes sim))
      ;; (js/console.log "(.alpha sim) " (.alpha sim))
      (.attr link-nodes "transform"
             (fn [_ i]
               (when (< i (.-length computed-links))
                 ;; (.log js/console "tick d " d)
                 ;; (.log js/console "tick link i " i)
                 ;; (.log js/console "tick nodes " nodes)
                 ;; (.log js/console "tick (.nodes sim) " (.nodes sim))
                 (let [
                       ;; _ (.log js/console "tick link nodes " link-nodes)
                       link (aget computed-links i)
                       ;; _ (.log js/console "tick computed link " link)
                       src-id (.. link -source -id)
                       src-cx (.. link -source -x)
                       src-cy (.. link -source -y)
                       dst-id (.. link -target -id)
                       dst-cx (.. link -target -x)
                       dst-cy (.. link -target -y)
                       src-b
                       (merge
                        (-> clj-graph
                            :bubbles
                            (get src-id))
                        {:cx src-cx :cy src-cy})
                       dst-b
                       (merge
                        (-> clj-graph
                            :bubbles
                            (get dst-id))
                        {:cx dst-cx :cy dst-cy})
                       ;; _ (js/console.log "src-id " src-id)
                       ;; _ (js/console.log "dst-id " dst-id)
                       ;; _ (js/console.log "src-b " src-b)
                       ;; _ (js/console.log "dst-b " dst-b)
                       [src-pt-x src-pt-y _ _]
                       (geometry/incidental-border-points-between-bubbles src-b dst-b)
                       th0
                       (-> (geometry/angle-between-bubbles src-b dst-b)
                           geometry/radian->degree)
                       ]
                   ;; (js/console.log "src-pt-x " src-pt-x)
                   ;; (js/console.log "src-pt-y " src-pt-y)
                   (str "translate(" src-pt-x " " src-pt-y ") "
                        "rotate(" th0 ")")
                   ))))

      ;; If the current graph is close enough to the previous one, stop the simulation
      (if (graph-converged? 0.01 @previous-nodes computed-nodes)
        (do
          (.debug js/console "TICK: DBG STOP SIMULATION")
          ;; Update the global application state
          (put! event-queue [:simulation-move computed-nodes])
          (.stop @current-simulation)
          (reset! previous-nodes nil)
          (reset! is-running? false))
        ;; Else, update the previous node positions
        (reset! previous-nodes computed-nodes))
      )

    ))

(defn- simulation [event-chan
                   cx-svg-user cy-svg-user
                   graph bubble-nodes link-nodes
                   clj-graph]
  (let [sim
        (-> js/d3
            (.forceSimulation)
            (.force "link"
                    (-> js/d3
                        (.forceLink)
                        (.id (fn [d] (.-id d)))
                        (.distance 200)
                        (.strength 0.4)))
            (.force "charge"
                    (-> js/d3
                        (.forceManyBody)
                        (.strength -2500)))
            (.force "center"
                    (.forceCenter js/d3 cx-svg-user cy-svg-user))
            (.force "collision"
                    (-> js/d3
            (.forceCollide 100)))
            ;; the gravity force relies on a custom implementation
            (.force "gravity"
                    (-> (gravity/force)
                        (.strength (* 0.125 500))
                        (.fixId "root")))
            (.nodes (.-nodes graph))
            (.on "end" (fn [] (js/console.debug "ON EVENT: END OF SIM")))
            )]

    (-> sim
        (.on "tick" (ticked event-chan sim bubble-nodes link-nodes clj-graph)))

    (-> sim
        (.force "link")
        (.links (.-links graph)))

    (reset! previous-nodes (js-node->cljs-node (.nodes sim)))

    sim))

(defn- build-nodes-field [appstate]
  (reduce
   (fn [acc [id {:keys [cx cy]}]]
     (conj acc {:id id :x cx :y cy :group 1}))
   []
   (state-read/get-bubbles appstate)))

(defn- build-links-field [appstate]
  (reduce
   (fn [acc {:keys [src dst]}]
     (conj acc {:source src :target dst :value 10}))
   []
   (state-read/get-links appstate)))

(defn launch-simulation! [appstate event-queue]
  (let [connected-graph (state-read/connected-graph appstate ROOT-BUBBLE-ID)
        graph
        {:nodes (build-nodes-field connected-graph)
         :links (build-links-field connected-graph)}
        ;; _ (js/console.log "appstate " appstate)
        ;; _ (js/console.log "connected-graph " connected-graph)
        ;; _ (js/console.log "graph " graph)
        ;; _ (js/console.log "(:links graph) " (:links graph))

        svg-node
        (-> js/d3
            (.select "#app svg"))
        bubble-nodes
        (-> svg-node
            (.select "#bubbles")
            (.selectAll ".bubble"))
        link-nodes
        (-> svg-node
            (.select "#links")
            (.selectAll ".link"))

        nb-nodes (-> connected-graph state-read/get-bubbles count)
        barycenter (state-read/graph-barycenter connected-graph)
        {bary-x :x bary-y :y} barycenter
        {camera-cx :cx camera-cy :cy} (camera/state-center)
        [cx cy] (->> (interleave [bary-x bary-y] [camera-cx camera-cy])
                     (partition 2)
                     (map #(apply + %))
                     (map #(/ % 2)))]
    (when @current-simulation
      (.stop @current-simulation))
    (when (< 1 nb-nodes)
      (reset! is-running? true)
      (reset! current-simulation
              (simulation event-queue cx cy
                          (clj->js graph) bubble-nodes link-nodes
                          connected-graph)))))


;; ;; BEGIN: DRAG SECTION

;; (defn- get-idx-by-id-js-node [nodes id]
;;   (->> nodes
;;        (map-indexed (fn [idx js-node] [(= (.-id js-node) id) idx]))
;;        (filter (fn [[bool _]] bool))
;;        first
;;        second))

;; (defn- simulation-drag-init! [appstate event-queue]
;;   (let [sim (launch-simulation! appstate event-queue)]
;;     (when-not (nil? sim)
;;       (-> sim
;;           (.alphaTarget 0.3)
;;           (.alpha 0.5)
;;           (.restart)))))

;; (def in-drag? (atom false))
;; (def drag-has-moved? (atom false))

;; (defn simulation-set-node-position [sim dragged-node-id fx fy]
;;   (let [js-nodes         (.nodes sim)
;;         idx-dragged-node (get-idx-by-id-js-node js-nodes dragged-node-id)
;;         js-node          (aget js-nodes idx-dragged-node)]
;;     (when js-node
;;       (set! (.-fx js-node) fx)
;;       (set! (.-fy js-node) fy))))

;; (defn simulation-set-node-in-place [sim dragged-node-id]
;;   (let [js-nodes         (.nodes sim)
;;         idx-dragged-node (get-idx-by-id-js-node js-nodes dragged-node-id)
;;         js-node          (aget js-nodes idx-dragged-node)]
;;     (when js-node
;;       (let [fx               (.-x js-node)
;;             fy               (.-y js-node)]
;;         (simulation-set-node-position sim dragged-node-id fx fy)))))

;; (defn simulation-drag-start! [dragged-node-id]
;;   (when-not (nil? @current-simulation)
;;     (reset! in-drag? true)
;;     (simulation-set-node-in-place @current-simulation dragged-node-id)))

;; (defn simulation-drag! [appstate dragged-node-id node-cx node-cy event-queue]
;;   (let [sim (if @drag-has-moved?
;;               @current-simulation
;;               (do
;;                 (reset! drag-has-moved? true)
;;                 (simulation-drag-init! appstate event-queue)))]
;;     (simulation-set-node-position sim dragged-node-id node-cx node-cy)))

;; (defn simulation-drag-end! [dragged-node-id]
;;   (when @drag-has-moved?
;;    (reset! in-drag? false)
;;    (reset! drag-has-moved? false)
;;    (let [sim @current-simulation]
;;      (simulation-set-node-position sim dragged-node-id nil nil)
;;      (-> sim
;;          (.alpha 0.3)
;;          (.alphaTarget 0)))))

;; ;; END: DRAG SECTION
