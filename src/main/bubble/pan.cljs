(ns bubble.pan
  (:require
   [bubble.camera :as camera]
   [bubble.coordinate :as coord]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

(defn apply-pan [mouse-pos-svg-px init-mouse-pos-svg-px init-cam-pos-svg-user]
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

(defn pan-move-fn []
  (let [{init-cam-cx-svg-user :cx init-cam-cy-svg-user :cy} (camera/state)]
    (fn [evt]
      (let [mouse-pos-svg-px
            (coord/win-px->svg-px [(.-clientX evt) (.-clientY evt)])]
        (when (or (nil? @init-cam-pos-svg-user)
                  (nil? @init-mouse-pos-svg-px))
          (reset! init-cam-pos-svg-user [init-cam-cx-svg-user init-cam-cy-svg-user])
          (reset! init-mouse-pos-svg-px mouse-pos-svg-px))

        (apply-pan mouse-pos-svg-px @init-mouse-pos-svg-px @init-cam-pos-svg-user)
        ))))

(defn pan-end-fn [pan-move pan-end-atom on-end]
  (fn []
    (events/unlisten js/window EventType.MOUSEMOVE pan-move)
    (events/unlisten js/window EventType.MOUSEUP @pan-end-atom)
    (on-end)
    (reset! init-cam-pos-svg-user nil)
    (reset! init-mouse-pos-svg-px nil)
    ))

(defn should-center
  "If the graph is not more visible, call the home event to 'center'
  the view around the graph."
  []
  ;; 10e-3: an arbitrary small value
  ;; TODO: adjust with the animation
  (when (not (camera/in-pan-limit? 10e-3))
    (camera/home-evt)))

(defn panning
  ([] (panning (fn []) should-center))
  ([on-start on-end]
   (let [pan-move (pan-move-fn)
         pan-end-atom (atom nil)
         pan-end (pan-end-fn pan-move pan-end-atom on-end)]
     (on-start)
     (reset! pan-end-atom pan-end)
     (events/listen js/window EventType.MOUSEMOVE pan-move)
     (events/listen js/window EventType.MOUSEUP pan-end))))
