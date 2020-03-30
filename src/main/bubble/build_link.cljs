(ns bubble.build-link
  (:require
   [bubble.coordinate :as coord]
   [bubble.event :as event]
   [bubble.state :as state]
   [cljs.core.async :refer [put!]]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
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
   (on-start)
   (put! event/event-queue [:build-link-start bubble-id mouse-x mouse-y])
   (events/listen js/window EventType.MOUSEMOVE build-link-move)))

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
  (fn []
    (build-link-end bubble-id)))
