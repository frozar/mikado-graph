(ns bubble.drag
  (:require [reagent.core :as reagent]
            [goog.events :as events]
            [bubble.event :as event]
            [bubble.state :as state]
            [bubble.geometry :as geom]
            [bubble.coordinate :as coord]
            [cljs.core.async :refer [put!]]
            )
  (:import [goog.events EventType]
           )
)

(defn drag-move-fn [bubble-id]
  (let [{:keys [center]} (state/get-bubble bubble-id)
        init-cx (geom/x center)
        init-cy (geom/y center)
        init-evt-x (atom nil)
        init-evt-y (atom nil)]
    (fn [evt]
      (let [[evt-x evt-y] (coord/get-svg-coord
                           (.-clientX evt) (.-clientY evt))
            ]
        (if (and (nil? @init-evt-x)
                 (nil? @init-evt-y))
          (do
            (reset! init-evt-x evt-x)
            (reset! init-evt-y evt-y))
          )
        (put! event/event-queue
              [:dragging
               bubble-id
               (+ init-cx (- evt-x @init-evt-x))
               (+ init-cy (- evt-y @init-evt-y))])
        ))))

(defn drag-end-fn [drag-move drag-end-atom on-end]
  (fn [evt]
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
      (if (if-left-click evt)
        (dragging bubble-id)))))
