(ns simulation.core
  (:require
   [bubble.camera :as camera]
   [bubble.state-read :as state-read]
   [cljs.core.async :refer [put!]]
   [cljsjs.d3]
   [clojure.walk :as walk]
   ["/d3/gravity" :as gravity]
   ))

(def current-simulation (atom nil))
(def previous-nodes (atom nil))

(defn stop-simulation! []
  (when @current-simulation
    (.stop @current-simulation))
  (reset! current-simulation nil))

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
          (stop-simulation!)
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
                    (->  js/d3
                         (.forceManyBody)
                         (.strength -2500)))
            (.force "center"
                    (.forceCenter js/d3 cx-svg-user cy-svg-user))
            (.force "collision"
                    (->  js/d3
                         (.forceCollide 100)))
            ;; the gravity force relies on a custom implementation
            (.force "gravity"
                    (->  (gravity/force)
                         (.strength (* 0.125 500))
                         (.fixId "root"))))]

    (-> sim
        (.nodes
         (.-nodes graph))
        (.on "tick"
             (ticked event-chan sim))
        (.on "end"
             (stop-simulation!))
        ;; (.alphaMin 0.1)
        )

    (-> sim
        (.force "link")
        (.links (.-links graph)))

    (reset! previous-nodes (js-node->cljs-node (.nodes sim)))

    sim))

(defn- appstate->graph [appstate]
  (let [nodes-field
        (reduce
         (fn [acc [id {:keys [cx cy]}]]
           (conj acc {:id id :x cx :y cy :group 1}))
         []
         (state-read/get-bubbles appstate))

        links-field
        (reduce
         (fn [acc {:keys [src dst]}]
           (conj acc {:source src :target dst :value 10}))
         []
         (state-read/get-links appstate))]
    {:nodes nodes-field
     :links links-field}))

(defn launch-simulation! [appstate event-queue]
  (let [graph (appstate->graph appstate)
        {:keys [cx cy]} (camera/state-center)]
    (stop-simulation!)
    (reset! current-simulation (simulation event-queue cx cy (clj->js graph))))
  #_(js/setTimeout
   (fn []
     (.log js/console "2 second")
     (when @current-simulation
       (.stop @current-simulation))
     )
   2000))
