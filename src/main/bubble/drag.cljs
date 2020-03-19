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
  (let [{:keys [center]} (state/get-bubble bubble-id)
        initial-cx (g/x center)
        initial-cy (g/y center)
        initial-evt-x (atom nil)
        initial-evt-y (atom nil)]
    (fn [evt]
      (let [{evt-x :x evt-y :y} (get-svg-coord
                                 @svg-bounding-box
                                 (.-clientX evt) (.-clientY evt))
            ]
        (if (and (nil? @initial-evt-x)
                 (nil? @initial-evt-y))
          (do
            (reset! initial-evt-x evt-x)
            (reset! initial-evt-y evt-y))
          )
        (put! event/event-queue
              [:dragging
               bubble-id
               (+ initial-cx (- evt-x @initial-evt-x))
               (+ initial-cy (- evt-y @initial-evt-y))])
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
