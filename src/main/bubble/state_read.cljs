(ns bubble.state-read
  (:require
   [bubble.state :refer [appstate]]
   [bubble.bubble :as bubble]
   [debux.cs.core :refer-macros [clog clogn dbg dbgn break]]
   ))

;; Read application state

(defn get-state []
  @appstate)

;; START: bubble part
(defn get-bubble
  ([id] (get-bubble @appstate id))
  ([appstate id]
   (-> appstate :bubbles (#(get % id)))))

(defn get-bubbles
  ([] (get-bubbles @appstate))
  ([appstate]
   (:bubbles appstate)))

(defn get-list-id
  ([] (get-list-id @appstate))
  ([appstate]
   (-> appstate :bubbles keys)))

(defn bubble-id-exist [appstate id]
  (let [idx (get-list-id appstate)]
    (not= (some #{id} idx) nil)))

(defn graph-bbox []
  (let [get-global-extremity
        (fn [bubble-characteristic-fn min-or-max-fn]
          (->> (get-bubbles)
               vals
               (map bubble-characteristic-fn)
               (apply min-or-max-fn)))]
    {:left   (get-global-extremity bubble/left-bubble   min)
     :right  (get-global-extremity bubble/right-bubble  max)
     :top    (get-global-extremity bubble/top-bubble    min)
     :bottom (get-global-extremity bubble/bottom-bubble max)}))

(defn- graph-bbox-dimension []
  (let [{left :left
         right :right
         top :top
         bottom :bottom} (graph-bbox)
        width (- right left)
        height (- bottom top)]
    {:width width :height height}))

(defn graph-bbox-area []
  (let [{width :width
         height :height} (graph-bbox-dimension)]
    (* width height)))

(defn graph-min-bubble-bbox-area []
  (->> (get-bubbles)
       vals
       (map #(bubble/bbox-area-bubble %))
       (apply min)))

(defn graph-mid-pt []
  (let [bbox (graph-bbox)
        top-left-pt [(:left bbox) (:top bbox)]
        bottom-right-pt [(:right bbox) (:bottom bbox)]
        mid-pt (->> (map + top-left-pt bottom-right-pt)
                    (map (fn [v] (/ v 2))))]
    mid-pt))

(defn graph-width-height []
  (let [bbox (graph-bbox)]
    {:width  (- (:right bbox) (:left bbox))
     :height (- (:bottom bbox) (:top bbox))}))

(defn graph-barycenter [appstate]
  (let [nb-bubbles (->> appstate
                        get-bubbles
                        count)]
    (->> appstate
         get-bubbles
         vals
         (map (fn [{:keys [cx cy]}] [cx cy]))
         (apply interleave)
         (split-at nb-bubbles)
         (map #(apply + %))
         (map #(/ % nb-bubbles))
         (#((fn [[x y]] {:x x :y y}) %)))))
;; END: bubble part

;; START: link part
(defn get-link-src []
  (:link-src @appstate))

(defn get-links
  ([]
   (get-links @appstate))
  ([appstate]
   (:links appstate)))

(defn link-exist [appstate src-id dst-id]
  (let [links (get-links appstate)]
    (not= (some #{{:src src-id :dst dst-id}} links) nil)))
;; END: link part

;; START: computer connected graph
(defn- reachable-nodes [appstate id]
  (loop [node-to-explore [id]
         node-visited []]
    (if (empty? node-to-explore)
      node-visited
      (let [current-id (first node-to-explore)
            new-id-node-to-explore
            (->> appstate
                 get-links
                 (filter (fn [m] (= (:src m) current-id)))
                 (map :dst))
            update-node-to-explore
            (apply merge (rest node-to-explore) new-id-node-to-explore)
            update-node-visited
            (conj node-visited current-id)
            ]
        (recur update-node-to-explore update-node-visited)))))

(defn connected-graph
  ([id] (connected-graph @appstate id))
  ([appstate id]
   (let [nodes-to-keep (reachable-nodes appstate id)]
     (-> appstate
         (update :bubbles #(select-keys % nodes-to-keep))
         (update :links #(filterv (fn [{src :src}] (some #{src} nodes-to-keep)) %))))))
;; END: computer connected graph

(defn get-mouse-position []
  (:mouse-position @appstate))

(defn get-rendering-style []
  (:rendering-style @appstate))
