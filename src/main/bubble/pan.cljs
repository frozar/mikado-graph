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
  (let [{init-cam-cx :cx init-cam-cy :cy zoom :zoom} @camera/camera
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

        (swap! camera/camera
               merge
               {:cx (- init-cam-cx (/ (- mouse-x @init-mouse-x) zoom))
                :cy (- init-cam-cy (/ (- mouse-y @init-mouse-y) zoom))
                })))))

(defn pan-end-fn [pan-move pan-end-atom on-end]
  (fn []
    (events/unlisten js/window EventType.MOUSEMOVE pan-move)
    (events/unlisten js/window EventType.MOUSEUP @pan-end-atom)
    (on-end)))

(defn panning
  ([] (panning (fn []) (fn [])))
  ([on-start on-end]
   (let [pan-move (pan-move-fn)
         pan-end-atom (atom nil)
         pan-end (pan-end-fn pan-move pan-end-atom on-end)]
     (on-start)
     (reset! pan-end-atom pan-end)
     (events/listen js/window EventType.MOUSEMOVE pan-move)
     (events/listen js/window EventType.MOUSEUP pan-end))))
