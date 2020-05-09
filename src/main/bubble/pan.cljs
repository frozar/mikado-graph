(ns bubble.pan
  (:require
   [bubble.camera :as camera]
   [bubble.coordinate :as coord]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

(defn pan-move-fn []
  (let [{init-cam-cx :cx init-cam-cy :cy} (camera/state)
        init-mouse-x-svg-px (atom nil)
        init-mouse-y-svg-px (atom nil)]
    (fn [evt]
      (let [[mouse-x-svg-px mouse-y-svg-px]
            (coord/win-px->svg-px [(.-clientX evt) (.-clientY evt)])
            ]
        (when (or (nil? @init-mouse-x-svg-px)
                  (nil? @init-mouse-y-svg-px))
          (reset! init-mouse-x-svg-px mouse-x-svg-px)
          (reset! init-mouse-y-svg-px mouse-y-svg-px))

        (let [new-camera
              (camera/update-camera
               {:cx (- init-cam-cx (camera/scale-dist (- mouse-x-svg-px @init-mouse-x-svg-px)))
                :cy (- init-cam-cy (camera/scale-dist (- mouse-y-svg-px @init-mouse-y-svg-px)))
                })]
          (camera/set-camera! new-camera)
          )
        ))))

(defn pan-end-fn [pan-move pan-end-atom on-end]
  (fn []
    (events/unlisten js/window EventType.MOUSEMOVE pan-move)
    (events/unlisten js/window EventType.MOUSEUP @pan-end-atom)
    (on-end)))

(defn should-center
  "If the graph is not more visible, call the home event to 'center'
  the view around the graph."
  []
  ;; 10e-3: an arbitrary small value
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
