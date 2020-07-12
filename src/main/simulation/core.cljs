(ns simulation.core
  (:require
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.state-read :as state-read]
   [cljs.core.async :refer [put!]]
   [cljsjs.d3]
   [clojure.walk :as walk]
   ["/d3/gravity" :as gravity]
   ))

(def current-simulation (atom nil))
(def previous-nodes (atom nil))

(defn- square-dist
  [{x0 :x y0 :y} {x1 :x y1 :y}]
  (let [square (fn [x] (* x x))]
    (+ (square (- x1 x0))
       (square (- y1 y0)))))

(defn- graph-distance
  "Return the sum of the square distances between each node of two graph."
  [previous-nodes nodes]
  (->>
   (map square-dist previous-nodes nodes)
   (apply +)))

(defn- graph-converged?
  "Return true if the distance between two graph is smaller than a given 'threshold'."
  [threshold previous-nodes nodes]
  (let [dist (graph-distance previous-nodes nodes)]
    (< dist threshold)))

(defn- js-node->cljs-node [nodes]
  (-> nodes
      js->clj
      walk/keywordize-keys))

(defn- ticked [event-queue sim]
  (fn tick []
    (let [nodes (js-node->cljs-node (.nodes sim))]
      ;; Move the nodes
      (put! event-queue [:simulation-move nodes])

      ;; If the current graph is close enough to the previous one, stop the simulation
      (if (graph-converged? 0.01 @previous-nodes nodes)
        (do
          (.debug js/console "TICK: DBG STOP SIMULATION")
          (.stop @current-simulation)
          (reset! previous-nodes nil))
        ;; Else, update the previous node positions
        (reset! previous-nodes nodes)))))

(defn- simulation [event-chan
                   cx-svg-user cy-svg-user
                   graph]
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
                        (.fixId "root"))))]

    (-> sim
        (.nodes
         (.-nodes graph))
        (.on "tick"
             (ticked event-chan sim))
        (.on "end"
             (fn []
               (.log js/console "ON EVENT: END OF SIM"))))

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

        nb-nodes (-> connected-graph state-read/get-bubbles count)
        barycenter (state-read/graph-barycenter connected-graph)
        {cx :x cy :y} barycenter
        ]
    (when @current-simulation
      (.stop @current-simulation))
    (when (< 1 nb-nodes)
      (reset! current-simulation (simulation event-queue cx cy (clj->js graph))))))

;; BEGIN: DRAG SECTION

(defn- get-idx-by-id-js-node [nodes id]
  (->> nodes
       (map-indexed (fn [idx js-node] [(= (.-id js-node) id) idx]))
       (filter (fn [[bool _]] bool))
       first
       second))

(defn- simulation-drag-init! [appstate event-queue]
  (let [sim (launch-simulation! appstate event-queue)]
    (-> sim
        (.alphaTarget 0.3)
        (.alpha 0.5)
        (.restart))))

(def in-drag? (atom false))
(def drag-has-moved? (atom false))

(defn simulation-set-node-position [sim dragged-node-id fx fy]
  (let [js-nodes         (.nodes sim)
        idx-dragged-node (get-idx-by-id-js-node js-nodes dragged-node-id)
        js-node          (aget js-nodes idx-dragged-node)]
    (set! (.-fx js-node) fx)
    (set! (.-fy js-node) fy)))

(defn simulation-set-node-in-place [sim dragged-node-id]
  (let [js-nodes         (.nodes sim)
        idx-dragged-node (get-idx-by-id-js-node js-nodes dragged-node-id)
        js-node          (aget js-nodes idx-dragged-node)
        fx               (.-x js-node)
        fy               (.-y js-node)]
    (simulation-set-node-position sim dragged-node-id fx fy)))

(defn simulation-drag-start! [dragged-node-id]
  (when-not (nil? @current-simulation)
    (reset! in-drag? true)
    (simulation-set-node-in-place @current-simulation dragged-node-id)))

(defn simulation-drag! [appstate dragged-node-id node-cx node-cy event-queue]
  (let [sim (if @drag-has-moved?
              @current-simulation
              (do
                (reset! drag-has-moved? true)
                (simulation-drag-init! appstate event-queue)))]
    (simulation-set-node-position sim dragged-node-id node-cx node-cy)))

(defn simulation-drag-end! [dragged-node-id]
  (when @drag-has-moved?
   (reset! in-drag? false)
   (reset! drag-has-moved? false)
   (let [sim @current-simulation]
     (simulation-set-node-position sim dragged-node-id nil nil)
     (-> sim
         (.alpha 0.3)
         (.alphaTarget 0)))))

;; END: DRAG SECTION
