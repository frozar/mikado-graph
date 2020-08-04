(ns simulation.core
  (:require
   [bubble.camera :as camera]
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.geometry :as geometry]
   [bubble.state-read :as state-read]
   [cljs.core.async :refer [put!]]
   [clojure.walk :as walk]
   ["/d3/gravity" :as gravity]
   ))

(def current-simulation (atom nil))
(def is-running? (atom false))

(defn- js-node->cljs-node [nodes]
  (-> nodes
      js->clj
      walk/keywordize-keys))

(defn update-app-state-bubble-position [event-queue]
  (when (and (not (nil? @current-simulation))
           @is-running?)
    (let [nodes (js-node->cljs-node (.nodes @current-simulation))]
      (put! event-queue [:simulation-move nodes]))))

(defn- compute-max-square-speed [js-nodes]
  (->> js-nodes
       .-length
       range
       (map (fn [i] (aget js-nodes i)))
       (map (fn [node]
              (+
               (* (.-vx node) (.-vx node))
               (* (.-vy node) (.-vy node)))))
       (apply max)))

(defn- graph-converged?
  "Return true if the speed of the fastest node of the graph is under a given 'threshold'."
  [threshold js-nodes]
  (< (compute-max-square-speed js-nodes) threshold))

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

(defn- compute-link-extremities [clj-graph computed-links i]
  (when (< i (.-length computed-links))
    (let [link (aget computed-links i)
          [src-b dst-b] (get-bubbles clj-graph link)]
      (geometry/incidental-border-points-between-bubbles src-b dst-b))))

(defn- ticked [event-queue sim clj-graph]
  (fn tick []
    (let [bubble-selection
          (-> js/d3
              (.select "#bubbles")
              (.selectAll ".bubble"))
          link-selection
          (-> js/d3
              (.select "#links")
              (.selectAll ".link"))
          computed-links (-> sim (.force "link") (.links))
          js-nodes (.nodes sim)]

      (.attr
       bubble-selection "transform"
       (fn [_ i]
         (when (< i (.-length js-nodes))
           (let [node (aget (.nodes sim) i)
                 translation-x (.-x node)
                 translation-y (.-y node)]
             (str "translate(" translation-x " " translation-y ")")))))

      (.attr link-selection "transform" nil)

      ;; At the first run of the tick function, remove the path tag inside link node
      ;; (the arrow in solid rendering mode)
      (let [path-selection
            (-> link-selection
                (.selectAll "path"))
            path-present? (not (nil? (-> path-selection (.node))))
            ]
        (when path-present?
          (.remove path-selection)
          (-> link-selection
              (.append "line")
              (.attr "stroke-width" 2)
              (.attr "stroke" "black"))))

      (let [line-selection
            (-> link-selection
                (.select "line"))]
        (.attr line-selection "x1"
               (fn [_ i]
                 (let [[x1 _ _ _] (compute-link-extremities clj-graph computed-links i)]
                   x1)))
        (.attr line-selection "y1"
               (fn [_ i]
                 (let [[_ y1 _ _] (compute-link-extremities clj-graph computed-links i)]
                   y1)))
        (.attr line-selection "x2"
               (fn [_ i]
                 (let [[_ _ x2 _] (compute-link-extremities clj-graph computed-links i)]
                   x2)))
        (.attr line-selection "y2"
               (fn [_ i]
                 (let [[_ _ _ y2] (compute-link-extremities clj-graph computed-links i)]
                   y2))))

      ;; (.stop @current-simulation)
      ;; If the nodes of the graph nearly don't move, stop the simulation
      (when (graph-converged? 5.0 (.nodes sim))
        (js/console.debug "TICK: DBG STOP SIMULATION")
        (js/console.debug "")
        ;; Update the global application state
        (put! event-queue [:simulation-move (js-node->cljs-node js-nodes)])
        (when @current-simulation
          (.stop @current-simulation))
        (reset! is-running? false)
        ))))

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
            )]

    (-> sim
        (.on "tick" (ticked event-chan sim clj-graph))
        (.on "end" (fn [] (js/console.debug "ON EVENT: END OF SIM"))))

    (-> sim
        (.force "link")
        (.links (.-links graph)))

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
