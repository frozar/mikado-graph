(ns bubble.drag
  (:require [reagent.core :as reagent]
            [goog.events :as events]
            [bubble.event :as event]
            [bubble.state :as state]
            [bubble.geometry :as g]
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
  {:x (- x (.-left bounding-client-rect))
   :y (- y (.-top bounding-client-rect))}
  )

(defn drag-move-fn [bubble-id]
  (let [initial-evt-coord (atom nil)
        {:keys [center]} (state/get-bubble bubble-id)
        initial-cx (g/x center)
        initial-cy (g/y center)]
    (fn [evt]
      (let [{:keys [x y]} (get-svg-coord @svg-bounding-box (.-clientX evt) (.-clientY evt))
            current-x-evt x
            current-y-evt y]
        (if (nil? @initial-evt-coord)
          (reset! initial-evt-coord {:x current-x-evt :y current-y-evt}))
        (put! event/event-queue
              [:dragging
               bubble-id
               (+ initial-cx (- current-x-evt (:x @initial-evt-coord)))
               (+ initial-cy (- current-y-evt (:y @initial-evt-coord)))])
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
