(ns bubble.build-link
  (:require
   [bubble.camera :as camera]
   [bubble.event :as event]
   [bubble.state-read :as state-read]
   [cljs.core.async :refer [put!]]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   )
  )

(defn get-mouse-svg-user-position [evt]
  (camera/win-px->svg-user [(.-clientX evt) (.-clientY evt)]))

(defn build-link-move [evt]
  (let [[mouse-x mouse-y] (get-mouse-svg-user-position evt)]
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
    (let [[mouse-x mouse-y] (get-mouse-svg-user-position evt)]
      (build-link-start bubble-id mouse-x mouse-y))))

(defn build-link-end
  ([bubble-id]
   (build-link-end bubble-id (fn [])))
  ([bubble-id on-end]
   (if-not (nil? (state-read/get-link-src))
     (do
       (events/unlisten js/window EventType.MOUSEMOVE build-link-move)
       (put! event/event-queue [:build-link-end bubble-id])
       (on-end)))))

(defn build-link-end-fn [bubble-id]
  (fn []
    (build-link-end bubble-id)))
