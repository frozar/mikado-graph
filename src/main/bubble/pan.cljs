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
        init-mouse-x (atom nil)
        init-mouse-y (atom nil)]
    (fn [evt]
      (let [[mouse-x mouse-y]
            (coord/win-px->svg-px [(.-clientX evt) (.-clientY evt)])
            ]
        (when (or (nil? @init-mouse-x)
                  (nil? @init-mouse-y))
          (reset! init-mouse-x mouse-x)
          (reset! init-mouse-y mouse-y))

        (let [new-camera
              (camera/update-camera
               {:cx (- init-cam-cx (camera/scale-dist (- mouse-x @init-mouse-x)))
                :cy (- init-cam-cy (camera/scale-dist (- mouse-y @init-mouse-y)))
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
