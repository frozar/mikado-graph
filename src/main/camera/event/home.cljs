(ns camera.event.home
  (:require
   [bubble.state-read :as state-read]
   [camera.core :as core]
   [camera.simulation :as simulation]
   [camera.state :as state]
   ))

;; BEGIN HOME-EVT ENVIRONMENT
(defn- current-time []
  (.now js/Date))

(defn- move-home-camera
  [target-camera
   current-camera current-x'-svg-user]
  (let [{cx-src-svg-user :cx cy-src-svg-user :cy zoom-src-svg-user :zoom} current-camera
        {cx-dst-svg-user :cx cy-dst-svg-user :cy zoom-dst-svg-user :zoom} target-camera

        current-x-svg-user
        (map -
             [cx-dst-svg-user cy-dst-svg-user zoom-dst-svg-user]
             [cx-src-svg-user cy-src-svg-user zoom-src-svg-user])

        [next-x-svg-user next-x'-svg-user]
        (->>
         (interleave current-x-svg-user current-x'-svg-user)
         (partition 2)
         (map (partial simulation/motion-solver 0.1 0.8 4 (/ 1 60)))
         (apply interleave)
         (partition 3)) ; 3 components: cx cy zoom

        [new-cx new-cy new-zoom]
        (map -
             [cx-dst-svg-user cy-dst-svg-user zoom-dst-svg-user]
             next-x-svg-user)

        new-camera (core/update-camera current-camera {:cx new-cx :cy new-cy :zoom new-zoom})]
    [new-camera next-x'-svg-user]))

(def home-camera-velocity (atom [0 0 0]))
(def home-evt-background-id (atom nil))
(def home-initial-time (atom nil))

(defn- reset-home-evt-environment! []
  (reset! home-camera-velocity [0 0 0])
  (reset! home-initial-time (current-time)))

(defn stop-background! []
  (when @home-evt-background-id
    (js/clearInterval @home-evt-background-id))
  (reset! home-evt-background-id nil)
  (reset-home-evt-environment!))

(defn- square-norm [vector]
  (apply +
         (map (fn [v] (* v v)) vector)))

(defn- move-home-camera! [target-camera]
  (let [elapsed-time (- (current-time) @home-initial-time)]
    (if (and
         (< 500 elapsed-time)  ; at least 0.5 second of animation
         (< (square-norm @home-camera-velocity) 10e-3))
      (stop-background!)
      (let [[new-camera x'-svg-user]
            (move-home-camera target-camera
                              @state/camera @home-camera-velocity)]
        (core/set-camera! new-camera)
        (reset! home-camera-velocity x'-svg-user)))))

(defn- start-background! [target-camera]
  (reset-home-evt-environment!)
  (reset! home-evt-background-id
          (js/setInterval move-home-camera! (/ 1000 60) target-camera)))

(defn- target-dimension->zoom
  "From a target width and height for the camera, compute the zoom associated"
  [target-dimension]
  (let [camera-dimension [(@state/camera :width) (@state/camera :height)]
        zooms (map / camera-dimension target-dimension)
        weakest_zoom (apply min zooms)]
    weakest_zoom))

(defn trigger-evt []
  (let [[cx cy] (state-read/graph-mid-pt)
        {:keys [width height]} (state-read/graph-width-height)
        border-factor 1.2
        target-dimension [(* border-factor width) (* border-factor height)]
        weakest_zoom (target-dimension->zoom target-dimension)

        target-camera (core/update-camera @state/camera {:cx cx :cy cy :zoom weakest_zoom})]
    (start-background! target-camera)))
;; END HOME-EVT ENVIRONMENT
