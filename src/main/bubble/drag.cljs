(ns bubble.drag
  (:require
   [bubble.coordinate :as coord]
   [bubble.event :as event]
   [bubble.state-read :as state-read]
   [cljs.core.async :refer [put!]]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   )
  )

(defn drag-move-fn [bubble-id]
  (let [{:keys [cx cy]} (state-read/get-bubble bubble-id)
        init-cx cx
        init-cy cy
        init-mouse-x (atom nil)
        init-mouse-y (atom nil)]
    (fn [evt]
      (let [[mouse-x mouse-y] (coord/get-svg-coord
                               (.-clientX evt) (.-clientY evt))
            ]
        (when (and (nil? @init-mouse-x)
                 (nil? @init-mouse-y))
          (reset! init-mouse-x mouse-x)
          (reset! init-mouse-y mouse-y)
          )
        (put! event/event-queue
              [:dragging
               bubble-id
               (+ init-cx (- mouse-x @init-mouse-x))
               (+ init-cy (- mouse-y @init-mouse-y))])
        ))))

(defn drag-end-fn [drag-move drag-end-atom on-end]
  (fn []
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (events/unlisten js/window EventType.MOUSEUP @drag-end-atom)
    (on-end)))

(defn dragging
  ([bubble-id] (dragging bubble-id (fn []) (fn [])))
  ([bubble-id on-start on-end]
   (let [drag-move (drag-move-fn bubble-id)
         drag-end-atom (atom nil)
         drag-end (drag-end-fn drag-move drag-end-atom on-end)]
     (on-start)
     (reset! drag-end-atom drag-end)
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))

(defn dragging-fn
  [bubble-id]
  (let [if-left-click
        (fn [evt]
          (= 0 (.-button evt)))]
    (fn [evt]
      (when (if-left-click evt)
        (dragging bubble-id)))))
