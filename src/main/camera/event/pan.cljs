(ns camera.event.pan
  (:require
   [bubble.coordinate :as coord]
   [bubble.state-read :as state-read]
   [camera.core :as core]
   [camera.simulation :as simulation]
   [camera.state :as state]
   [cljs.core.async :refer [put!]]
   ))

;; BEGIN CAMERA MOTION
(defn- move-camera
  [initial-camera initial-mouse-svg-px target-mouse-svg-px
   current-camera current-x'-svg-px]
  (let [target-translation-vec-svg-px
        (map - initial-mouse-svg-px target-mouse-svg-px)

        {cx0-svg-user :cx cy0-svg-user :cy} initial-camera
        {cx1-svg-user :cx cy1-svg-user :cy} current-camera

        center-initial-camera-svg-px
        (core/svg-user->svg-px initial-camera [cx0-svg-user cy0-svg-user])

        center-current-camera-svg-px
        (core/svg-user->svg-px initial-camera [cx1-svg-user cy1-svg-user])

        center-target-camera-svg-px
        (map + target-translation-vec-svg-px center-initial-camera-svg-px)

        current-x-svg-px
        (map - center-current-camera-svg-px center-target-camera-svg-px)

        [next-x-svg-px next-x'-svg-px]
        (->>
         (interleave current-x-svg-px current-x'-svg-px)
         (partition 2)
         (map (partial simulation/motion-solver 0.1 0.8 4 (/ 1 60)))
         (apply interleave)
         (partition 2))

        center-next-camera-svg-px
        (map + center-target-camera-svg-px next-x-svg-px)

        ;; center-next-camera-svg-user
        [new-cx new-cy]
        (core/svg-px->svg-user initial-camera center-next-camera-svg-px)

        new-camera (core/update-camera current-camera {:cx new-cx :cy new-cy})]
    [new-camera next-x'-svg-px]))

(defn- move-camera-without-motion-solver
  [initial-camera initial-mouse-svg-px target-mouse-svg-px
   current-camera]
  (let [target-translation-vec-svg-px
        (map - initial-mouse-svg-px target-mouse-svg-px)

        {cx0-svg-user :cx cy0-svg-user :cy} initial-camera

        center-initial-camera-svg-px
        (core/svg-user->svg-px current-camera [cx0-svg-user cy0-svg-user])

        center-next-camera-svg-px
        (map + center-initial-camera-svg-px target-translation-vec-svg-px)

        ;; center-next-camera-svg-user
        [new-cx new-cy]
        (core/svg-px->svg-user current-camera center-next-camera-svg-px)

        new-camera (core/update-camera current-camera {:cx new-cx :cy new-cy})]
    new-camera))
;; END CAMERA MOTION

;; BEGIN PANNING ENVIRONMENT
(def initial-camera (atom nil))
(def camera-velocity (atom [0 0]))
(def initial-mouse-svg-px (atom nil))
(def target-mouse-svg-px (atom [0 0]))
(def panning-background-id (atom nil))
(def minimal-intersect-over-view 20)
(def minimal-intersect-over-graph 20)

(defn- move-camera! []
  (let [[new-camera x'-svg-px]
        (move-camera @initial-camera @initial-mouse-svg-px @target-mouse-svg-px
                          @state/camera @camera-velocity)]
    (core/set-camera! new-camera)
    (reset! camera-velocity x'-svg-px)))

(defn- pan-start-background! []
  (reset! panning-background-id
          (js/setInterval move-camera! (/ 1000 60))))

(defn- pan-stop-background! []
  (when @panning-background-id
    (js/clearInterval @panning-background-id))
  (reset! panning-background-id nil))

(defn- set-pan-environment! [mouse-pos-svg-px]
  (reset! initial-camera @state/camera)
  (reset! initial-mouse-svg-px mouse-pos-svg-px)
  (reset! camera-velocity [0 0])
  (reset! target-mouse-svg-px mouse-pos-svg-px))

(defn- reset-pan-environment! []
  (reset! initial-camera nil)
  (reset! initial-mouse-svg-px nil)
  (reset! camera-velocity [0 0])
  (reset! target-mouse-svg-px [0 0]))
;; END PANNING ENVIRONMENT

(defn- in-pan-limit?
  "Check if the graph is still enough in the displayed viewBox. Typically,
  if the intersection area (between graph and view) represents less than 20% of
  the view bbox area, than return false."
  [camera minimal-intersect-over-graph minimal-intersect-over-view]
  (let [view-bbox (core/camera->view-bbox camera)
        {view-left :left
         view-right :right
         view-top :top
         view-bottom :bottom} view-bbox

        {graph-left :left graph-right :right
         graph-top :top graph-bottom :bottom} (state-read/graph-bbox)

        intersection-bbox
        {:left (max view-left graph-left)
         :right (min view-right graph-right)
         :top (max view-top graph-top)
         :bottom (min view-bottom graph-bottom)}

        intersection-bbox-area (core/bbox-area intersection-bbox)
        view-bbox-area (core/bbox-area view-bbox)
        graph-bbox-area (state-read/graph-bbox-area)

        intersect-over-view-ratio (* 100 (/ intersection-bbox-area view-bbox-area))
        intersect-over-graph-ratio (* 100 (/ intersection-bbox-area graph-bbox-area))]
    (or (= view-bbox intersection-bbox)
        (< minimal-intersect-over-view intersect-over-view-ratio)
        (< minimal-intersect-over-graph intersect-over-graph-ratio))))

(defn- should-trigger-home-evt?
  "If the graph is no more visible, call the home event to 'center'
  the view around the graph."
  []
  (when (not (in-pan-limit? @state/camera minimal-intersect-over-graph minimal-intersect-over-view))
    (put! state/event-queue [:home])))

(defmulti panning (fn [panning-type panning-action _] [panning-type panning-action]))

(defmethod panning [:standard :start]
  [_ _ mouse-pos-win-px]
  (let [mouse-pos-svg-px (coord/win-px->svg-px mouse-pos-win-px)]
    (set-pan-environment! mouse-pos-svg-px)))

(defmethod panning [:standard :move]
  [_ _ mouse-pos-win-px]
  (let [current-mouse-svg-px (coord/win-px->svg-px mouse-pos-win-px)
        new-camera
        (move-camera-without-motion-solver
         @initial-camera @initial-mouse-svg-px current-mouse-svg-px
         @state/camera)]
    (core/set-camera! new-camera)))

(defmethod panning [:standard :stop]
  [_ _ _]
  (should-trigger-home-evt?)
  (reset-pan-environment!))

(defmethod panning [:animated :start]
  [_ _ mouse-pos-win-px]
  (let [mouse-pos-svg-px (coord/win-px->svg-px mouse-pos-win-px)]
    (set-pan-environment! mouse-pos-svg-px)
    (pan-start-background!)))

(defmethod panning [:animated :move]
  [_ _ mouse-pos-win-px]
  (let [current-mouse-svg-px (coord/win-px->svg-px mouse-pos-win-px)]
    (reset! target-mouse-svg-px current-mouse-svg-px)))

(defmethod panning [:animated :stop]
  [_ _ _]
  (pan-stop-background!)
  (should-trigger-home-evt?)
  (reset-pan-environment!))
