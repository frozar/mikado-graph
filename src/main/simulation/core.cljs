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
   [simulation.state :refer [current-simulation] ;; :as state
    ]
   ))

(def is-running? (atom false))

(defn- js-node->cljs-node [nodes]
  (-> nodes
      js->clj
      walk/keywordize-keys))

(defn update-app-state-bubble-position [event-queue]
  (when (and
         (not (nil? @current-simulation))
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

(defn- ticked [event-queue sim clj-graph]
  (fn tick []
    ;; (js/console.debug "begin tick")
    (let [js-nodes (.nodes sim)
          computed-links (-> sim (.force "link") (.links))
          bubble-selection
          (-> js/d3
              (.select "#bubbles")
              (.selectAll ".bubble")
              (.data js-nodes)
              )
          link-selection
          (-> js/d3
              (.select "#links")
              (.selectAll ".link"))]

      (.attr
       bubble-selection "transform"
       (fn [d]
         (let [translation-x (.-x d)
               translation-y (.-y d)]
           (str "translate(" translation-x " " translation-y ")"))))

      ;; At the first run of the tick function, remove the path tag inside link node
      ;; (the arrow in solid rendering mode)
      (let [links-dom-element
            (js/document.getElementsByClassName "link")
            links-dom-child-element
            (map
             (fn [idx]
               (-> links-dom-element
                   (.item idx)
                   (.-childNodes)))
             (range (.-length links-dom-element)))]
        ;; (js/console.log "links-dom-child-element " links-dom-child-element)
        ;; (js/console.log
        ;;  "count children "
        ;;  (->> links-dom-child-element
        ;;       (map (fn [child-nodes] (.-length child-nodes)))
        ;;       ))
        (when (->> links-dom-child-element
                   (map (fn [child-nodes] (.-length child-nodes)))
                   (filter (fn [n] (not (= n 1))))
                   empty?
                   #(not %))
          ;; (js/console.log "IN when")
          ;; Delete DOM node link content
          (doseq [child-nodes links-dom-child-element]
            (-> child-nodes
                (.forEach (fn [child-node]
                            (.remove child-node))
                          )))

          ;; Add lines
          (-> link-selection
              (.append "line")
              (.attr "stroke-width" 2)
              (.attr "stroke" "black"))))

      (.attr
       link-selection
       "transform"
       (fn [_ i]
         (when (< i (.-length computed-links))
           (let [link (aget computed-links i)
                 [src-b dst-b] (get-bubbles clj-graph link)

                 [src-pt-x src-pt-y _ _]
                 (geometry/incidental-border-points-between-bubbles src-b dst-b)
                 rad-th0 (geometry/angle-between-bubbles src-b dst-b)
                 deg-th0 (geometry/radian->degree rad-th0)]
             (str "translate(" src-pt-x " " src-pt-y ") "
                  "rotate(" deg-th0 ")")))))

      (let [line-selection
            (-> link-selection
                (.select "line"))]
        (-> line-selection
            (.attr "x1" 0)
            (.attr "y1" 0)
            (.attr "y2" 0)
            (.attr
             "x2"
             (fn [_ i]
               (when (< i (.-length computed-links))
                 (let [link (aget computed-links i)
                       [src-b dst-b] (get-bubbles clj-graph link)
                       [x1 y1 x2 y2] (geometry/incidental-border-points-between-bubbles src-b dst-b)
                       arrow-length (geometry/dist x1 y1 x2 y2)]
                   arrow-length))))))

      ;; (.stop @current-simulation)
      ;; If the nodes of the graph nearly don't move, stop the simulation
      (when (graph-converged? 1.0 (.nodes sim))
        (js/console.debug "TICK: DBG STOP SIMULATION")
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
        (.on "end"
             (fn []
               (js/console.debug "ON EVENT: END OF SIM")
               (update-app-state-bubble-position event-chan))))

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

(defn- get-idx-by-id-js-node [nodes id]
  (->> nodes
       (map-indexed (fn [idx js-node] [(= (.-id js-node) id) idx]))
       (filter (fn [[bool _]] bool))
       first
       second))

(defn simulation-set-node-position [sim node-id fx fy]
  (let [js-nodes         (.nodes sim)
        idx-dragged-node (get-idx-by-id-js-node js-nodes node-id)
        js-node (aget js-nodes idx-dragged-node)]
    (when js-node
      (set! (.-fx js-node) fx)
      (set! (.-fy js-node) fy))))

(defn simulation-drag! [appstate dragged-node-id node-cx node-cy event-queue]
  (let [sim (if (nil? @current-simulation)
              (launch-simulation! appstate event-queue)
              @current-simulation)]
    (simulation-set-node-position sim dragged-node-id node-cx node-cy)
    (-> sim
        (.alpha 1)
        (.alphaTarget 0.3)
        (.restart))))

(defn simulation-drag-end! [dragged-node-id]
  (let [sim @current-simulation]
    (simulation-set-node-position sim dragged-node-id nil nil)
    (-> sim
        (.alpha 0.3)
        (.alphaTarget 0)
        (.restart))))

;; ;; END: DRAG SECTION
