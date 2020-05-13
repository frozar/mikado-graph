(ns bubble.pan
  (:require
   [bubble.camera :as camera]
   [bubble.coordinate :as coord]
   [cljs.core.async :refer [put!]]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

(defn pan-move* [mouse-pos-svg-px init-mouse-pos-svg-px init-cam-pos-svg-user]
  (let [new-camera-center
        (->> mouse-pos-svg-px
             (#(map - % init-mouse-pos-svg-px))
             (map #(camera/scale-dist %))
             (#(map - init-cam-pos-svg-user %))
             (zipmap [:cx :cy]))

        new-camera
        (camera/update-camera new-camera-center)
        ]
    (camera/set-camera! new-camera)
    ))

(def init-cam-pos-svg-user (atom nil))
(def init-mouse-pos-svg-px (atom nil))

(defn pan-move []
  (let [{init-cam-cx-svg-user :cx init-cam-cy-svg-user :cy} (camera/state)]
    (fn [evt]
      (let [mouse-pos-svg-px
            (coord/win-px->svg-px [(.-clientX evt) (.-clientY evt)])
            mouse-pos-win-px [(.-clientX evt) (.-clientY evt)]]
        (put! camera/event-queue [:pan-move mouse-pos-win-px])

        (when (or (nil? @init-cam-pos-svg-user)
                  (nil? @init-mouse-pos-svg-px))
          (reset! init-cam-pos-svg-user [init-cam-cx-svg-user init-cam-cy-svg-user])
          (reset! init-mouse-pos-svg-px mouse-pos-svg-px))

        (pan-move* mouse-pos-svg-px @init-mouse-pos-svg-px @init-cam-pos-svg-user)
        ))))

(defn pan-move-2 []
  (fn [evt]
    (let [mouse-pos-win-px [(.-clientX evt) (.-clientY evt)]]
      (put! camera/event-queue [:pan-move mouse-pos-win-px]))))

(defn pan-end [pan-move ref-to-pan-end-atom on-end]
  (fn []
    (events/unlisten js/window EventType.MOUSEMOVE pan-move)
    (events/unlisten js/window EventType.MOUSEUP @ref-to-pan-end-atom)
    (put! camera/event-queue [:pan-stop])
    (reset! init-cam-pos-svg-user nil)
    (reset! init-mouse-pos-svg-px nil)
    (on-end)
    ))

(defn pan-start [evt]
  (let [;; mouse-pos-svg-user
        ;; (camera/win-px->svg-user [(.-clientX evt) (.-clientY evt)])
        mouse-pos-win-px [(.-clientX evt) (.-clientY evt)]]
    (put! camera/event-queue [:pan-start mouse-pos-win-px])))

(defn should-center
  "If the graph is not more visible, call the home event to 'center'
  the view around the graph."
  []
  ;; 10e-3: an arbitrary small value
  ;; TODO: adjust with the animation
  (when (not (camera/in-pan-limit? 10e-3))
    (camera/home-evt)))

(defn panning
  ([evt] (panning evt (fn []) should-center))
  ([evt hook-on-start hook-on-end]
   (let [;; a-pan-move (pan-move)
         a-pan-move (pan-move-2)
         ref-to-pan-end (atom nil)
         a-pan-end (pan-end a-pan-move ref-to-pan-end hook-on-end)]
     (hook-on-start)
     (pan-start evt)
     (reset! ref-to-pan-end a-pan-end)
     (events/listen js/window EventType.MOUSEMOVE a-pan-move)
     (events/listen js/window EventType.MOUSEUP a-pan-end))))
