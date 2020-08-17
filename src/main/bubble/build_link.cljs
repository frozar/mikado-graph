(ns bubble.build-link
  (:require
   [bubble.state-read :as state-read]
   [camera.core :as camera]
   [cljs.core.async :refer [put!]]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

(defn get-mouse-svg-user-position [evt]
  (camera/win-px->svg-user [(.-clientX evt) (.-clientY evt)]))

(defn build-link-move-fn [event-queue]
  (fn [evt]
    (let [[mouse-x mouse-y] (get-mouse-svg-user-position evt)]
      (put! event-queue [:build-link-move mouse-x mouse-y]))))

(def build-link-move (atom nil))
(def build-link-exit-evt (atom nil))

(defn- build-link-exit-evt-fn
  "Configure the press to escape-key to exit interactive edition mode.
  Currently the only interaction is with the build-link action."
  [event-queue build-link-move-arg]
  (fn self
    [evt]
    (condp = (.-key evt)
      "Escape"
      (do
        (events/unlisten js/window EventType.KEYDOWN self)
        (events/unlisten js/window EventType.MOUSEMOVE build-link-move-arg)
        (reset! build-link-move nil)
        (reset! build-link-exit-evt nil)
        (put! event-queue [:build-link-exit]))

      nil)))

(defn build-link-start
  ([event-queue bubble-id mouse-x mouse-y]
   (build-link-start event-queue bubble-id mouse-x mouse-y (fn [])))
  ([event-queue bubble-id mouse-x mouse-y on-start]
   (on-start)
   (put! event-queue [:build-link-start bubble-id mouse-x mouse-y])
   (reset! build-link-move (build-link-move-fn event-queue))
   (reset! build-link-exit-evt (build-link-exit-evt-fn event-queue @build-link-move))
   (events/listen js/window EventType.MOUSEMOVE @build-link-move)
   (events/listen js/window EventType.KEYDOWN @build-link-exit-evt)))

(defn build-link-start-fn [event-queue bubble-id]
  (fn [evt]
    (let [[mouse-x mouse-y] (get-mouse-svg-user-position evt)]
      (build-link-start event-queue bubble-id mouse-x mouse-y))))

(defn build-link-end
  ([event-queue bubble-id]
   (build-link-end event-queue bubble-id (fn [])))
  ([event-queue bubble-id on-end]
   (if-not (nil? (state-read/get-link-src))
     (do
       (when-not (nil? @build-link-move)
         (events/unlisten js/window EventType.MOUSEMOVE @build-link-move))
       (when-not (nil? @build-link-exit-evt)
         (events/unlisten js/window EventType.KEYDOWN @build-link-exit-evt))
       (reset! build-link-move nil)
       (reset! build-link-exit-evt nil)
       (put! event-queue [:build-link-end bubble-id])
       (on-end)))))
