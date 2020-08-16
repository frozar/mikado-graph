(ns camera.pan
  (:require
   [camera.state :as state]
   [cljs.core.async :refer [put!]]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

(defn- pan-move []
  (fn [evt]
    (let [mouse-pos-win-px [(.-clientX evt) (.-clientY evt)]]
      (put! state/event-queue [:pan-move mouse-pos-win-px]))))

(defn- pan-end [pan-move ref-to-pan-end-atom on-end]
  (fn []
    (events/unlisten js/window EventType.MOUSEMOVE pan-move)
    (events/unlisten js/window EventType.MOUSEUP @ref-to-pan-end-atom)
    (put! state/event-queue [:pan-stop])
    (on-end)
    ))

(defn- pan-start [evt]
  (let [mouse-pos-win-px [(.-clientX evt) (.-clientY evt)]]
    (put! state/event-queue [:pan-start mouse-pos-win-px])))

(defn panning
  ([evt] (panning evt (fn []) (fn [])))
  ([evt hook-on-start hook-on-end]
   (let [a-pan-move (pan-move)
         ref-to-pan-end (atom nil)
         a-pan-end (pan-end a-pan-move ref-to-pan-end hook-on-end)]
     (hook-on-start)
     (pan-start evt)
     (reset! ref-to-pan-end a-pan-end)
     (events/listen js/window EventType.MOUSEMOVE a-pan-move)
     (events/listen js/window EventType.MOUSEUP a-pan-end))))
