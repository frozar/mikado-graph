(ns camera.core
  (:require
   [bubble.coordinate :as coord]
   [bubble.state-read :as state-read]
   [camera.simulation :as simulation]
   [camera.state :as state]
   [cljs.core.async :refer [put!]]
   [clojure.string :as string]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

;; BEGIN CAMERA SECTION
(defn get-state []
  @state/camera)

(defn state-center []
  (select-keys @state/camera [:cx :cy]))

(defn state-dimension []
  (select-keys @state/camera [:width :height]))

(defn- camera->viewBox
  "Return the viewBox SVG property. Coord express in SVG user space."
  [camera]
  (let [width  (/ (:width camera)  (:zoom camera))
        height (/ (:height camera) (:zoom camera))
        min-x (- (:cx camera) (/ width 2.))
        min-y (- (:cy camera) (/ height 2.))]
    {:width width :height height :min-x min-x :min-y min-y}))

(defn camera->view-bbox
  ([] (camera->view-bbox @state/camera))
  ([camera]
   (let [{width :width height :height
          view-left :min-x view-top :min-y} (camera->viewBox camera)
         [view-right view-bottom] (map + [view-left view-top] [width height])
         view-bbox
         {:left view-left
          :right view-right
          :top view-top
          :bottom view-bottom}]
     view-bbox)))

(defn camera->viewBox-str []
  (let [{width :width
         height :height
         min-x :min-x
         min-y :min-y}
        (camera->viewBox @state/camera)]
    (string/join " " [min-x min-y width height])))

(defn camera-viewBox-area [camera]
  (let [{width :width height :height} (camera->viewBox camera)]
    (* width height)))

(declare svg-px->svg-user)
(declare svg-user->svg-px)

(defn- get-top-left-corner-svg-user [camera]
  (let [{:keys [min-x min-y]} (camera->viewBox camera)
        ;; vector in svg-user space: origin = pt (0, 0) in user space
        top-left-corner-svg-user [min-x min-y]]
    top-left-corner-svg-user))

(defn- correct-camera-by-translation-fix-point-svg-px
  [camera-origin camera-modified pt-svg-px]
  (let [pt-origin-svg-user   (svg-px->svg-user camera-origin pt-svg-px)
        pt-modified-svg-user (svg-px->svg-user camera-modified pt-svg-px)
        vec-trans-svg-user (map - pt-modified-svg-user pt-origin-svg-user)
        {:keys [cx cy]} camera-modified
        [new-cx new-cy] (map - [cx cy] vec-trans-svg-user)
        camera-corrected (merge camera-modified {:cx new-cx :cy new-cy})]
    camera-corrected))

(defn- apply-zoom
  "Invariant: the svg-user coordinates associated with the input svg-px must match
  after the camera update, in this case after a zoom factor update."
  [camera scale-factor pt-svg-px]
  (let [camera-zoomed (update camera :zoom * scale-factor)]
    (correct-camera-by-translation-fix-point-svg-px camera camera-zoomed pt-svg-px)))

(defn- apply-resize
  "Invariant: the svg-user coordinates associated with the input svg-px must match
  after the camera update, in this case after a width/height scalar update."
  [camera win-width win-height]
  (let [camera-origin camera
        camera-wider (merge camera-origin {:width win-width :height win-height})
        ;; Take an arbitrary point in the plan as a fix point
        pt-svg-px [0 0]]
    (correct-camera-by-translation-fix-point-svg-px camera camera-wider pt-svg-px)))

(defn update-camera
  ([hashmap]
   (update-camera @state/camera hashmap))
  ([camera hashmap]
   (merge camera hashmap)))

(defn dist-svg-px->dist-svg-user
  "Convert distance from SVG pixel space to SVG user space"
  ([dist]
   (dist-svg-px->dist-svg-user @state/camera dist))
  ([camera dist]
   (let [zoom (:zoom camera)]
     (/ dist zoom))))

(defn dist-svg-user->dist-svg-px
  "Convert distance from SVG user space to SVG pixel space"
  ([dist]
   (dist-svg-user->dist-svg-px @state/camera dist))
  ([camera dist]
   (let [zoom (:zoom camera)]
     (* dist zoom))))

(defn area-ratio-min-bubble<->viewBox
  "Return the pourcentage of the smallest bubble over the viewBox area."
  [camera]
  (let [bubble-bbox-area (state-read/graph-min-bubble-bbox-area)
        viewBox-area (camera-viewBox-area camera)
        ]
    (* 100 (/ bubble-bbox-area viewBox-area))))

(defn- in-zoom-limit?
  "Check the arbitrary limit of zoom in/out"
  [camera]
  (and (< 0.001 (area-ratio-min-bubble<->viewBox camera))
       (< (area-ratio-min-bubble<->viewBox camera) 50)))

(defn- bbox-area [{left :left right :right top :top bottom :bottom}]
  (let [width (- right left)
        height (- bottom top)]
    (if (or (neg? width) (neg? height))
      0
      (* width height))))

(defn in-pan-limit?
  "Check if the graph is still enough in the displayed viewBox. Typically,
  if the intersection area (between graph and view) represents less than 20% of
  the view bbox area, than return false."
  [camera minimal-intersect-over-graph minimal-intersect-over-view]
  (let [view-bbox (camera->view-bbox camera)
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

        intersection-bbox-area (bbox-area intersection-bbox)
        view-bbox-area (bbox-area view-bbox)
        graph-bbox-area (state-read/graph-bbox-area)

        intersect-over-view-ratio (* 100 (/ intersection-bbox-area view-bbox-area))
        intersect-over-graph-ratio (* 100 (/ intersection-bbox-area graph-bbox-area))]
    (or (= view-bbox intersection-bbox)
        (< minimal-intersect-over-view intersect-over-view-ratio)
        (< minimal-intersect-over-graph intersect-over-graph-ratio))))

(defn set-camera! [new-camera]
  (let [is-in-zoom-limit (in-zoom-limit? new-camera)]
    (when is-in-zoom-limit
      (reset! state/camera new-camera))))
;; END CAMERA SECTION

;; BEGIN COORDINATE CONVERTION
(defn svg-px->svg-user
  "From svg pixel position, convert to svg user position."
  [camera pt-svg-px]
  (let [;; vector in svg-user space: origin = (top, left) corner  in user space
        svg-scaled (map / pt-svg-px [(:zoom camera) (:zoom camera)])

        ;; vector in svg-user space: origin = pt (0, 0) in user space
        top-left-corner-svg-user (get-top-left-corner-svg-user camera)

        ;; change of landmark in svg-user space: origin = pt (0, 0) in user space
        pt-svg-user (map + svg-scaled top-left-corner-svg-user)
        ]
    pt-svg-user))

(defn svg-user->svg-px
  "From svg user position, convert to svg pixel position."
  [camera pt-svg-user]
  (let [;; vector in svg-user space: origin = pt (0, 0) in user space
        top-left-corner-svg-user (get-top-left-corner-svg-user camera)

        ;; change of landmark in svg-user space: origin = (top, left) corner in user space
        svg-scaled (map - pt-svg-user top-left-corner-svg-user)

        ;; vector in svg-px space: origin = (top, left) corner in window frame space
        pt-svg-px (map * svg-scaled [(:zoom camera) (:zoom camera)])
        ]
    pt-svg-px))

(defn win-px->svg-user
  "From window pixel position, convert to svg user position."
  ([pt-win-px] (win-px->svg-user @state/camera pt-win-px))
  ([camera pt-win-px]
   (let [svg-px (coord/win-px->svg-px pt-win-px)

         pt-user-coord (svg-px->svg-user camera svg-px)]
     pt-user-coord
     )))
;; END COORDINATE CONVERTION

(defn mouse-wheel [wheel-delta-y win-px-x win-px-y]
  (let [reduction-speed-factor 5
        scale (.pow js/Math 1.005 (/ wheel-delta-y reduction-speed-factor))
        svg-px (coord/win-px->svg-px [win-px-x win-px-y])
        new-camera (apply-zoom @state/camera scale svg-px)]
    (set-camera! new-camera)))

(defn- mouse-wheel-evt [evt]
  (put! state/event-queue
        [:mouse-wheel (..  evt -event_ -wheelDeltaY) (.-clientX evt) (.-clientY evt)]))

(defn mouse-wheel-evt-on []
  (events/listen js/window EventType.WHEEL mouse-wheel-evt))

(defn mouse-wheel-evt-off []
  (events/unlisten js/window EventType.WHEEL mouse-wheel-evt))

(defn window-resize [width height]
  (let [new-camera
        (apply-resize @state/camera width height)]
    (set-camera! new-camera)))

(defn- window-resize-evt []
  (put! state/event-queue
        [:resize (.-innerWidth js/window) (.-innerHeight js/window)]))

(defn window-resize-evt-on []
  (events/listen js/window EventType.RESIZE window-resize-evt))

(defn window-resize-evt-off []
  (events/unlisten js/window EventType.RESIZE window-resize-evt))

;; BEGIN CAMERA MOTION
(defn- move-camera
  [initial-camera initial-mouse-svg-px target-mouse-svg-px
   current-camera current-x'-svg-px]
  (let [target-translation-vec-svg-px
        (map - initial-mouse-svg-px target-mouse-svg-px)

        {cx0-svg-user :cx cy0-svg-user :cy} initial-camera
        {cx1-svg-user :cx cy1-svg-user :cy} current-camera

        center-initial-camera-svg-px
        (svg-user->svg-px initial-camera [cx0-svg-user cy0-svg-user])

        center-current-camera-svg-px
        (svg-user->svg-px initial-camera [cx1-svg-user cy1-svg-user])

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
        (svg-px->svg-user initial-camera center-next-camera-svg-px)

        new-camera (update-camera current-camera {:cx new-cx :cy new-cy})]
    [new-camera next-x'-svg-px]))

(defn- move-camera-without-motion-solver
  [initial-camera initial-mouse-svg-px target-mouse-svg-px
   current-camera]
  (let [target-translation-vec-svg-px
        (map - initial-mouse-svg-px target-mouse-svg-px)

        {cx0-svg-user :cx cy0-svg-user :cy} initial-camera

        center-initial-camera-svg-px
        (svg-user->svg-px current-camera [cx0-svg-user cy0-svg-user])

        center-next-camera-svg-px
        (map + center-initial-camera-svg-px target-translation-vec-svg-px)

        ;; center-next-camera-svg-user
        [new-cx new-cy]
        (svg-px->svg-user current-camera center-next-camera-svg-px)

        new-camera (update-camera current-camera {:cx new-cx :cy new-cy})]
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
    (set-camera! new-camera)
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

;; BEGIN CAMERA EVENT QUEUE
(defn should-trigger-home-evt?
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
    (set-camera! new-camera)))

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
;; END CAMERA EVENT QUEUE
