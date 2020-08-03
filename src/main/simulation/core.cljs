(ns simulation.core
  (:require
   [bubble.camera :as camera]
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.geometry :as geometry]
   [bubble.gui-solid :as gui-solid]
   [bubble.state-read :as state-read]
   [cljs.core.async :refer [put!]]
   [cljsjs.d3]
   [clojure.edn :as edn]
   [clojure.string :as string]
   [clojure.walk :as walk]
   ["/d3/gravity" :as gravity]
   [reagent.dom :as rdom]
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

(defn- get-bubbles [clj-graph link]
  (let [src-id (.. link -source -id)
        src-cx (.. link -source -x)
        src-cy (.. link -source -y)
        dst-id (.. link -target -id)
        dst-cx (.. link -target -x)
        dst-cy (.. link -target -y)

        origin-src-b (-> clj-graph
                         :bubbles
                         (get src-id))
        origin-dst-b (-> clj-graph
                         :bubbles
                         (get dst-id))

        src-b (merge
               origin-src-b
               {:cx src-cx :cy src-cy})
        dst-b (merge
               origin-dst-b
               {:cx dst-cx :cy dst-cy})]
    [src-b dst-b]))

(defn- ticked [event-queue sim clj-graph]
  (fn tick []
    ;; (js/console.log "BEGIN")
    (let [svg-node
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
          computed-nodes (js-node->cljs-node (.nodes sim))
          computed-links (-> sim (.force "link") (.links))
          line-nodes (-> js/d3
                         (.select "#app svg")
                         (.select "#links")
                         (.selectAll ".link")
                         (.select "line"))

          ]
      (.attr
       bubble-nodes "transform"
       (fn [_ i]
         (when (< i (-> computed-nodes keys count))
           (let [node (aget (.nodes sim) i)
                 translation-x (.-x node)
                 translation-y (.-y node)]
             (str "translate(" translation-x " " translation-y ")")))))

      (.attr
       link-nodes "transform"
       nil
       #_(fn [_ i]
         (when (< i (.-length computed-links))
           (let [link (aget computed-links i)

                 [src-b dst-b] (get-bubbles clj-graph link)

                 [src-pt-x src-pt-y _ _]
                 (geometry/incidental-border-points-between-bubbles src-b dst-b)

                 th0 (-> (geometry/angle-between-bubbles src-b dst-b)
                         geometry/radian->degree)]
             #_(str "translate(" src-pt-x " " src-pt-y ") "
                  "rotate(" th0 ")")
             ""))))

      ;; (.html
      ;;  link-nodes
      ;;  (fn [_ i]
      ;;    (when (< i (.-length computed-links))
      ;;      (let [link (aget computed-links i)

      ;;            [src-b dst-b] (get-bubbles clj-graph link)

      ;;            on-fly (.createElement js/document "svg")
      ;;            _ (rdom/render [gui-solid/draw-link src-b dst-b] on-fly)

      ;;            childNodes (-> on-fly
      ;;                           .-firstChild
      ;;                           .-childNodes)
      ;;            concatenation-innerHTML (->> childNodes
      ;;                                         .-length
      ;;                                         range
      ;;                                         (map
      ;;                                          (fn [i]
      ;;                                            (->> i
      ;;                                                 (aget childNodes)
      ;;                                                 .-outerHTML)))
      ;;                                         (apply str))]
      ;;        concatenation-innerHTML))))

      (let [path-present?
            (-> js/d3
                (.select "#app svg")
                (.select "#links")
                (.selectAll ".link")
                (.selectAll "path")
                (.node))]
        (when-not (nil? path-present?)
          ;; (js/console.log "path-present?: " true)
          (-> js/d3
              (.select "#app svg")
              (.select "#links")
              (.selectAll ".link")
              (.selectAll "path")
              (.remove)
              )
          (-> js/d3
              (.select "#app svg")
              (.select "#links")
              (.selectAll ".link")
              (.append "line")
              (.attr "stroke-width" 5)
              (.attr "stroke" "black")
              )))

      ;; (js/console.log "ticked config link"
      ;;                 (-> js/d3
      ;;                     (.select "#app svg")
      ;;                     (.select "#links")
      ;;                     (.selectAll ".link")
      ;;                     (.nodes)))

      ;; (-> js/d3
      ;;     (.select "#app svg")
      ;;     (.select "#links")
      ;;     (.selectAll ".link")
      ;;     (.selectAll "path")
      ;;     (.remove)
      ;;     )

      ;; (.attr
      ;;  (-> js/d3
      ;;      (.select "#app svg")
      ;;      (.select "#links")
      ;;      (.selectAll ".link")
      ;;      (.select "line"))
      ;;  "stroke-width"
      ;;  5)
      ;; (.attr
      ;;  (-> js/d3
      ;;      (.select "#app svg")
      ;;      (.select "#links")
      ;;      (.selectAll ".link")
      ;;      (.select "line"))
      ;;  "stroke"
      ;;  "black")

      (.attr
       (-> js/d3
           (.select "#app svg")
           (.select "#links")
           (.selectAll ".link")
           (.select "line"))
       "x1"
       (fn [_ i]
         (when (< i (.-length computed-links))
           (let [link (aget computed-links i)

                 [src-b dst-b] (get-bubbles clj-graph link)

                 [src-pt-x _ _ _]
                 (geometry/incidental-border-points-between-bubbles src-b dst-b)]
             src-pt-x))))
      (.attr
       (-> js/d3
           (.select "#app svg")
           (.select "#links")
           (.selectAll ".link")
           (.select "line"))
       "y1"
       (fn [_ i]
         (when (< i (.-length computed-links))
           (let [link (aget computed-links i)

                 [src-b dst-b] (get-bubbles clj-graph link)

                 [_ src-pt-y _ _]
                 (geometry/incidental-border-points-between-bubbles src-b dst-b)]
             src-pt-y))))
      (.attr
       (-> js/d3
           (.select "#app svg")
           (.select "#links")
           (.selectAll ".link")
           (.select "line"))
       "x2"
       (fn [_ i]
         (when (< i (.-length computed-links))
           (let [link (aget computed-links i)

                 [src-b dst-b] (get-bubbles clj-graph link)

                 [_ _ dst-pt-x  _]
                 (geometry/incidental-border-points-between-bubbles src-b dst-b)]
             dst-pt-x))))
      (.attr
       (-> js/d3
           (.select "#app svg")
           (.select "#links")
           (.selectAll ".link")
           (.select "line"))
       "y2"
       (fn [_ i]
         (when (< i (.-length computed-links))
           (let [link (aget computed-links i)

                 [src-b dst-b] (get-bubbles clj-graph link)

                 [_ _ _ dst-pt-y]
                 (geometry/incidental-border-points-between-bubbles src-b dst-b)]
             dst-pt-y))))

      ;; (.stop @current-simulation)
      ;; (js/console.log "END")
      ;; If the current graph is close enough to the previous one, stop the simulation
      (if (graph-converged? 0.01 @previous-nodes computed-nodes)
        (do
          (js/console.debug "TICK: DBG STOP SIMULATION")
          ;; (-> js/d3
          ;;     (.select "#app svg")
          ;;     (.select "#links")
          ;;     (.selectAll ".link")
          ;;     (.selectAll "line")
          ;;     (.remove)
          ;;     )
          (js/console.debug "TICK: DBG AFTER REMOVE")
          (js/console.debug "")
          ;; Update the global application state
          (put! event-queue [:simulation-move computed-nodes])
          (.stop @current-simulation)
          (reset! previous-nodes nil)
          (reset! is-running? false))
        ;; Else, update the previous node positions
        (reset! previous-nodes computed-nodes))
      )))

(defn- simulation [event-chan
                   cx-svg-user cy-svg-user
                   graph
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
        (.on "tick" (ticked event-chan sim clj-graph)))

    (-> sim
        (.force "link")
        (.links (.-links graph)))

    (reset! previous-nodes (js-node->cljs-node (.nodes sim)))
    ;; (-> js/d3
    ;;     (.select "#app svg")
    ;;     (.select "#links")
    ;;     (.selectAll ".link")
    ;;     (.remove)
    ;;     (.append "line"))

    ;; (-> js/d3
    ;;     (.select "#app svg")
    ;;     (.select "#links")
    ;;     (.selectAll ".link")
    ;;     (.selectAll "path")
    ;;     (.remove)
    ;;     )

    ;; (-> js/d3
    ;;     (.select "#app svg")
    ;;     (.select "#links")
    ;;     (.selectAll ".link")
    ;;     (.select
    ;;      (fn [_ i nodes]
    ;;        ;; (js/console.log "this " this)
    ;;        (js/console.log "node " (aget nodes i))))
    ;;     )
    ;; (rdom/unmount-component-at-node container)

    ;; (-> js/d3
    ;;     (.select "#app svg")
    ;;     (.select "#links")
    ;;     (.selectAll ".link")
    ;;     (.append "line")
    ;;     )

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
        ;; _ (js/console.log "graph links" (graph :links))

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
                          (clj->js graph)
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
