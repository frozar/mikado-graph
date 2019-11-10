(ns svg-ellipse.bubble
  (:require [reagent.core :as reagent]
            [svg-ellipse.geometry :as g]
            [goog.events :as events]
            [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]
            )
  (:import [goog.events EventType]
           )
  )

  (def root-id "root")

(defonce points
  (reagent/atom
   {
    :bubbles [{:id root-id
               :c (g/point 250 450)
               :rx 100
               :ry 50
               }]
    :links []
    }))

(defn get-bubble [id]
  (first (filter #(= (:id %) id) (:bubbles @points))))

(defn gen-id []
  "Generate a string of length 8, e.g.: 'b56d74c5'"
  (apply str (repeatedly 8 #(rand-nth "0123456789abcdef"))))

(defn get-bcr [svg-root]
  (-> svg-root
      reagent/dom-node
      .getBoundingClientRect))

(defn get-svg-coord [svg-root x y]
  (let [bcr (get-bcr svg-root)]
    {:x (- x (.-left bcr)) :y (- y (.-top bcr))}))

;; (defn get-svg-coord-fn [svg-root]
;;   (let [bcr (get-bcr svg-root)]
;;     (clog bcr)
;;     (fn [x y]
;;       {:x (- x (.-left bcr)) :y (- y (.-top bcr))})))

(defn move-bubble [svg-root id]
  ;; (clog id)
  ;; (clog (:bubbles @points))
  ;; (clog (map #(:id %) (:bubbles @points)))
  (fn [x y]
    (let [bcr (get-bcr svg-root)]
      ;; (js/console.log bcr)
      ;; (clog (.-left bcr))
      ;; (clog (.-right bcr))
      (swap!
       points update :bubbles
       (fn [list-bubble]
         (let [list-idxs (map #(:id %) list-bubble)]
           ;; (clog id)
           ;; (clog list-idxs)
           ;; (clog (.indexOf list-idxs id))
           ;; (clog (.-left bcr))
           ;; (clog (- x (.-left bcr)))
           ;; Check if the id to delete is present in the model.
           ;; When the user drag a bubble on a right click, the move action
           ;; associated try to delete an id which was ready deleted.
           (if (some #{id} list-idxs)
             (update
              list-bubble
              (.indexOf list-idxs id)
              ;; #(merge % {:c (g/point (- x (.-left bcr)) (- y (.-top bcr)))}))
              ;; #(merge % {:c (g/point x (- y (.-top bcr)))}))
              #(merge % {:c (g/point x y)}))
              ;; #(merge % {:c (g/point x y)}))
             ;; Else body
             list-bubble)))))))

(def ellipse-defaults
  {
   :fill "#f06"
   :stroke "black"
   :stroke-width 5
   })

;; (defn drag-move-fn [on-drag cx cy get-svg-coord]
(defn drag-move-fn [on-drag cx cy svg-root]
  (let [first-evt-coord (atom nil)]
    (fn [evt]
      ;; (let [{:keys [x y]} (get-svg-coord (.-clientX evt) (.-clientY evt))
      (let [{:keys [x y]} (get-svg-coord svg-root (.-clientX evt) (.-clientY evt))
            current-x-evt x
            current-y-evt y]
        (if (nil? @first-evt-coord)
          (reset! first-evt-coord {:x current-x-evt :y current-y-evt}))
        (on-drag (+ cx (- current-x-evt (:x @first-evt-coord)))
                 (+ cy (- current-y-evt (:y @first-evt-coord))))))))

(defn drag-end-fn [drag-move drag-end on-end]
  (fn [evt]
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (events/unlisten js/window EventType.MOUSEUP @drag-end)
    (on-end)))

(defn dragging
  ;; ([on-drag cx cy get-svg-coord] (dragging on-drag (fn []) (fn []) cx cy get-svg-coord))
  ([on-drag cx cy svg-root] (dragging on-drag (fn []) (fn []) cx cy svg-root))
  ;; ([on-drag on-start on-end cx cy get-svg-coord]
  ([on-drag on-start on-end cx cy svg-root]
   ;; (js/console.log "dragging")
   ;; (let [drag-move (drag-move-fn on-drag cx cy get-svg-coord)
   (let [drag-move (drag-move-fn on-drag cx cy svg-root)
         drag-end-atom (atom nil)
         drag-end (drag-end-fn drag-move drag-end-atom on-end)]
     (on-start)
     (reset! drag-end-atom drag-end)
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))

(defn if-left-click [evt]
  (= 0 (.-button evt)))

;; (defn dragging-fn [on-drag bubble get-svg-coord]
(defn dragging-fn [on-drag bubble svg-root]
  (fn [evt]
    (if (if-left-click evt)
      ;; (dragging on-drag (g/x (:c bubble)) (g/y (:c bubble)) get-svg-coord))))
      (dragging on-drag (g/x (:c bubble)) (g/y (:c bubble)) svg-root))))

(defn new-bubble [parent-bubble-id cx cy]
  (let [bubble-id (gen-id)]
    (swap! points update :bubbles conj {:id bubble-id :c (g/point cx cy) :rx 100 :ry 50})
    ;; (clog parent-bubble-id)
    ;; (clog bubble-id)
    ;; (clog (get-bubble parent-bubble-id))
    ;; (clog (get-bubble bubble-id))
    (swap! points update :links conj {:src parent-bubble-id :dst bubble-id})
    ;; (clog (:links @points))
    ))

(defn delete-bubble [id]
  (fn [evt]
    (.preventDefault evt)
    (swap! points update :bubbles (fn [l] (filterv #(not (= (:id %) id)) l)))
    ;; (clog id)
    ;; (clog (:links @points))
    (swap! points update :links (fn [l] (filterv
                                         (fn [link] not (= (some #{id} (vals link)) nil)) l)))
    ;; (clog (:links @points))
    ))

(defn draw-root-bubble [svg-root]
  ;; (clog (:bubbles @points))
  (let [root-bubble (get-bubble root-id)
        {:keys [c rx ry]} root-bubble
        basic-option {:cx (g/x c) :cy (g/y c)}
        on-drag (move-bubble svg-root root-id)]
    ;; (clog c)
    ;; [:<>
    [:g {
         :on-mouse-down (dragging-fn on-drag root-bubble svg-root)
         :on-double-click #(new-bubble (:id root-bubble) (g/x c) (- (g/y c) (* 3 ry)))
         :on-context-menu (fn [evt] (.preventDefault evt))
         }
     [:ellipse
      (merge ellipse-defaults basic-option
             {:rx (+ 10 rx) :ry (+ 10 ry)})]
     [:ellipse
      (merge ellipse-defaults basic-option
             {:rx rx :ry ry})]
     ]
    )
  )

(defn draw-bubble [svg-root bubble]
  (let [{:keys [id c rx ry]} bubble
        on-drag (move-bubble svg-root (:id bubble))]
    [:ellipse
     (merge ellipse-defaults
            {:key id
             :on-mouse-down (dragging-fn on-drag bubble svg-root)
             :on-double-click #(new-bubble (:id bubble) (g/x c) (- (g/y c) (* 3 ry)))
             :on-context-menu (delete-bubble id)
             :cx (g/x c)
             :cy (g/y c)
             :rx rx
             :ry ry
             })]))

(defn get-link-path [link]
  (let [{:keys [src dst]} link
        src-b (get-bubble src)
        dst-b (get-bubble dst)
        src-pt (:c src-b)
        dst-pt (:c dst-b)
        path-str (str "M " (g/x src-pt) "," (g/y src-pt) " L " (g/x dst-pt) "," (g/y dst-pt))]
    {:key (str (:id src-b) (:id dst-b))
     :stroke-width 4
     :stroke "black"
     :fill "none"
     :d path-str}
    )
  )

(defn draw-links []
  ;; (clog (:links @points))
  ;; (clog (map (fn [link] (map #(get-bubble %) link)) (:links @points)))
  ;; (clog (map (fn [link] (get-link-path link)) (:links @points)))
  (let [links-path (doall (map (fn [link] (get-link-path link)) (:links @points)))]
    ;; (clog links-path)
    [:g
     (for [path links-path]
       [:path path])
     ]
    )
  )

(defn all-bubble [svg-root]
  ;; (js/console.log "on-drag" on-drag)
  ;; (js/console.log @points)
  ;; (clog svg-root)
  ;; (js/console.log svg-root)
  [:g
   (draw-links)
   (draw-root-bubble svg-root)
   (for [bubble (filter #(not (= (:id %) root-id)) (:bubbles @points))]
     (draw-bubble svg-root bubble)
     )
   ])
