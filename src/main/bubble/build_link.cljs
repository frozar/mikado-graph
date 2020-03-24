(ns bubble.build-link
  (:require [goog.events :as events]
            [bubble.event :as event]
            [bubble.state :as state]
            [bubble.coordinate :as coord]
            [cljs.core.async :refer [put!]]
            )
  (:import [goog.events EventType]
           )
  )

(defn get-mouse-position [evt]
  (coord/get-svg-coord
   (.-clientX evt) (.-clientY evt)))

(defn build-link-move [evt]
  (let [[mouse-x mouse-y] (get-mouse-position evt)
        ]
    (put! event/event-queue [:build-link-move mouse-x mouse-y])))

(defn build-link-start
  ([bubble-id mouse-x mouse-y]
   (build-link-start bubble-id mouse-x mouse-y (fn [])))
  ([bubble-id mouse-x mouse-y on-start]
   (do
     (on-start)
     (put! event/event-queue [:build-link-start bubble-id mouse-x mouse-y])
     (events/listen js/window EventType.MOUSEMOVE build-link-move)
     )))

(defn build-link-start-fn [bubble-id]
  (fn [evt]
    (let [[mouse-x mouse-y] (get-mouse-position evt)]
      (build-link-start bubble-id mouse-x mouse-y))))

(defn build-link-end
  ([bubble-id]
   (build-link-end bubble-id (fn [])))
  ([bubble-id on-end]
   (if-not (nil? (state/get-link-src))
     (do
       (events/unlisten js/window EventType.MOUSEMOVE build-link-move)
       (put! event/event-queue [:build-link-end bubble-id])
       (on-end)))))

(defn build-link-end-fn [bubble-id]
  (fn [evt]
    (build-link-end bubble-id)))
