(ns bubble.pan
  (:require
   [bubble.camera :as camera]
   [bubble.coordinate :as coord]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

(defn- current-time []
  (.now js/Date))

(defn- delta-time [starting-time current-time]
  (- current-time starting-time))

(defn pan-move-fn []
  (let [{init-cam-cx-svg-user :cx init-cam-cy-svg-user :cy} (camera/state)
        init-cam-pos-svg-user [init-cam-cx-svg-user init-cam-cy-svg-user]
        init-mouse-pos-svg-px (atom nil)
        mouse-position (atom [])
        starting-time (atom nil)]
    (fn [evt]
      (let [mouse-pos-svg-px
            (coord/win-px->svg-px [(.-clientX evt) (.-clientY evt)])]
        (when (nil? @init-mouse-pos-svg-px)
          (reset! init-mouse-pos-svg-px mouse-pos-svg-px))

        (if (nil? @starting-time)
          (do
            (reset! starting-time (current-time))
            (swap! mouse-position conj [mouse-pos-svg-px 0]))
          (let [[_ previous-spent-time] (last @mouse-position)
                spent-time (delta-time @starting-time (current-time))]
            ;; 500: 500 millisecond
            (when (< 500 (delta-time previous-spent-time spent-time))
              (swap! mouse-position conj [mouse-pos-svg-px spent-time])))
          )

        ;; (prn [mouse-x-svg-px mouse-y-svg-px])
        ;; (prn "mouse-position" @mouse-position)

        (let [new-camera-center
              (->> mouse-pos-svg-px
                   (#(map - % @init-mouse-pos-svg-px))
                   (map #(camera/scale-dist %))
                   (#(map - init-cam-pos-svg-user %))
                   (zipmap [:cx :cy]))
              ;; _ (prn "new-camera-center" new-camera-center)

              new-camera
              (camera/update-camera new-camera-center)
              ]
          (camera/set-camera! new-camera)
          ;; (camera/animate-camera-transition new-camera 2) ;; ca fout la merde
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
  ;; (prn "now" (.now js/Date))
  ;; (prn "(not (camera/in-pan-limit? 10e-3)" (not (camera/in-pan-limit? 10e-3)))
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
