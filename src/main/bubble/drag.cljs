(ns bubble.drag
  (:require [reagent.core :as reagent]
            [goog.events :as events]
            [bubble.event :as event]
            [cljs.core.async :refer [chan put! <! go-loop]]
            )
  (:import [goog.events EventType]
           )
)

(defonce svg-bounding-box (reagent/atom nil))

(defn init-svg-bounding-box [bounding-client-svg-node]
  (reset! svg-bounding-box bounding-client-svg-node))

(defn get-svg-coord
  [bounding-client-rect x y]
  {:x (- x (.-left bounding-client-rect)) :y (- y (.-top bounding-client-rect))}
  )

(defn drag-move-fn [on-drag bubble-id cx cy]
  (let [first-evt-coord (atom nil)]
    (fn [evt]
      (let [{:keys [x y]} (get-svg-coord @svg-bounding-box (.-clientX evt) (.-clientY evt))
            current-x-evt x
            current-y-evt y]
        (if (nil? @first-evt-coord)
          (reset! first-evt-coord {:x current-x-evt :y current-y-evt}))
        ;; (on-drag (+ cx (- current-x-evt (:x @first-evt-coord)))
        ;;          (+ cy (- current-y-evt (:y @first-evt-coord))))
        (put! event/event-queue
              [:dragging
               bubble-id
               (+ cx (- current-x-evt (:x @first-evt-coord)))
               (+ cy (- current-y-evt (:y @first-evt-coord)))])
        ))))

(defn drag-end-fn [drag-move drag-end-atom on-end]
  (fn [evt]
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (events/unlisten js/window EventType.MOUSEUP @drag-end-atom)
    (on-end)))

(defn dragging
  ([on-drag bubble-id cx cy] (dragging on-drag bubble-id (fn []) (fn []) cx cy))
  ([on-drag bubble-id on-start on-end cx cy]
   (let [drag-move (drag-move-fn on-drag bubble-id cx cy)
         drag-end-atom (atom nil)
         drag-end (drag-end-fn drag-move drag-end-atom on-end)]
     (on-start)
     (reset! drag-end-atom drag-end)
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))

(defn dragging-fn
  [on-drag bubble-id center-x center-y]
  (let [if-left-click (fn [evt]
                        (= 0 (.-button evt)))]
    (fn [evt]
      (if (if-left-click evt)
        (dragging on-drag bubble-id center-x center-y)))))
