(ns bubble.build-link
  (:require
   [bubble.camera :as camera]
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

(defn get-mouse-position [evt]
  (let [window-coord [(.-clientX evt) (.-clientY evt)]
        svg-px (coord/window->svg-canvas-px window-coord)
        ;; _ (prn "DBG svg-px" svg-px)
        svg-scaled (map / svg-px [(:zoom @camera/camera) (:zoom @camera/camera)])
        ;; _ (prn "DBG svg-scaled" svg-scaled)

        mid-plan-px (map / [(:width @camera/camera) (:height @camera/camera)] [2 2])
        mid-plan-scaled (map / mid-plan-px [(:zoom @camera/camera) (:zoom @camera/camera)])

        pt-update-origin (map + svg-scaled [(:cx @camera/camera) (:cy @camera/camera)])
        pt-user-coord (map - pt-update-origin mid-plan-scaled)
        ]
    pt-user-coord
    ))

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
   (if-not (nil? (state-read/get-link-src))
     (do
       (events/unlisten js/window EventType.MOUSEMOVE build-link-move)
       (put! event/event-queue [:build-link-end bubble-id])
       (on-end)))))

(defn build-link-end-fn [bubble-id]
  (fn []
    (build-link-end bubble-id)))
