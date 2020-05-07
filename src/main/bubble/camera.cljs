(ns bubble.camera
  (:require
   [bubble.coordinate :as coord]
   [bubble.state-read :as state-read]
   [clojure.string :as string]
   [reagent.core :as reagent]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

;; BEGIN camera section
(defn init-camera
  ([]
   (let [width (.-innerWidth js/window)
         height (.-innerHeight js/window)]
     (init-camera width height)))
  ([width height]
   {:cx (/ width 2.)
    :cy (/ height 2.)
    :width width
    :height height
    :zoom 1.}))

(defonce camera
  (reagent/atom (init-camera)))

(defn- vec-camera [src-camera dst-camera]
  (let [distance-values (map - (vals dst-camera) (vals src-camera))
        vec-camera (zipmap (keys src-camera) distance-values)]
    vec-camera))

(defn- subpart-camera [camera set-of-keys]
  (reduce
   (fn [hashmap [k v]]
     (if (some set-of-keys [k])
       (conj hashmap [k v])
       hashmap))
   {}
   camera))

(def camera-subset #{:cx :cy :zoom})

(defn- div-camera [src-camera divisor]
  (let [sub-camera (subpart-camera src-camera camera-subset)
        divided-values (map (fn [v] (/ v divisor)) (vals sub-camera))
        inc-camera (zipmap (keys sub-camera) divided-values)]
    inc-camera))

(defn- add-camera [src-camera inc-camera]
  (let [set-of-keys (into #{} (keys inc-camera))
        incremented-camera
        (reduce
         (fn [hashmap [key value]]
           (if (some set-of-keys [key])
             (let [updated-val (+ value (get inc-camera key))]
               (conj hashmap [key updated-val]))
             (conj hashmap [key value])
             ))
         {}
         src-camera)]
    incremented-camera))


(defn- camera-linear-interpolation
  [src-camera dst-camera nb-step]
  (if (< nb-step 2)
    []
    (let [dist-camera (vec-camera src-camera dst-camera)
          divisor (dec nb-step)
          inc-camera (div-camera dist-camera divisor)]
      (take nb-step
            (iterate (fn [camera] (add-camera camera inc-camera)) src-camera))
      )))

(declare svg-px->svg-user-coord)

(defn- correct-camera-by-translation-fix-point-svg-px
  [camera-origin camera-modified pt-svg-px]
  (let [pt-origin-svg-user   (svg-px->svg-user-coord camera-origin pt-svg-px)
        pt-modified-svg-user (svg-px->svg-user-coord camera-modified pt-svg-px)
        vec-trans-svg-user (map - pt-modified-svg-user pt-origin-svg-user)
        {:keys [cx cy]} camera-modified
        [new-cx new-cy] (map - [cx cy] vec-trans-svg-user)
        camera-corrected (merge camera-modified {:cx new-cx :cy new-cy})]
    camera-corrected))

(defn- apply-zoom
  "Invariant: the svg-user coordinates associated with the input svg-px must match
  after the camera update, in this case after a zoom factor update."
  [camera scale pt-svg-px]
  (let [camera-zoomed (update camera :zoom * scale)]
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

(defn- update-camera! [new-camera]
  (reset! camera new-camera))
;; END camera section

(defn- camera->viewBox [camera]
  (let [width  (/ (:width camera)  (:zoom camera))
        height (/ (:height camera) (:zoom camera))
        min-x (- (:cx camera) (/ width 2.))
        min-y (- (:cy camera) (/ height 2.))]
    {:width width :height height :min-x min-x :min-y min-y}))

(defn camera->viewBox-str []
  (let [{width :width
         height :height
         min-x :min-x
         min-y :min-y}
        (camera->viewBox @camera)]
    (string/join " " [min-x min-y width height])))

(defn svg-px->svg-user-coord
  "From svg pixel position, convert to svg user position."
  [camera svg-px]
  (let [;; vector in svg-user space: origin = (top, left) corner
        svg-scaled (map / svg-px [(:zoom camera) (:zoom camera)])

        {:keys [min-x min-y]} (camera->viewBox camera)
        ;; vector in svg-user space: origin = pt (0, 0) in user space
        top-left-corner-svg-user [min-x min-y]

        ;; change of landmark in svg-user space: origin = pt (0, 0) in user space
        pt-user-coord (map + svg-scaled top-left-corner-svg-user)
        ]
    pt-user-coord))

(defn win-px->svg-user-coord
  "From window pixel position, convert to svg user position."
  [camera win-px]
  (let [svg-px (coord/win-px->svg-px win-px)

        pt-user-coord (svg-px->svg-user-coord camera svg-px)]
    pt-user-coord
    ))

(defn- mouse-wheel-evt [evt]
  (let [reduction-speed-factor (if (.-shiftKey evt) 10 5)
        scale (.pow js/Math 1.005 (/ (..  evt -event_ -wheelDeltaY) reduction-speed-factor))
        win-px [(.-clientX evt) (.-clientY evt)]
        new-camera (apply-zoom @camera scale (coord/win-px->svg-px win-px))]
    (update-camera! new-camera)))

(defn mouse-wheel-evt-fn []
  (events/listen js/window EventType.WHEEL mouse-wheel-evt))

(defn mouse-wheel-evt-off []
  (events/unlisten js/window EventType.WHEEL mouse-wheel-evt))

(defn- window-resize-evt []
  (let [new-camera
        (apply-resize @camera (.-innerWidth js/window) (.-innerHeight js/window))]
    (update-camera! new-camera)))

(defn window-resize-evt-fn []
  (events/listen js/window EventType.RESIZE window-resize-evt))

(defn window-resize-evt-off []
  (events/unlisten js/window EventType.RESIZE window-resize-evt))

(defn animate-camera-transition
  "duration: in second"
  ([src-camera dst-camera duration]
   (animate-camera-transition src-camera dst-camera duration 30))
  ([src-camera dst-camera duration fps]
   (let [nb-step (* duration fps)
         list-camera (camera-linear-interpolation src-camera dst-camera nb-step)
         ;; the time between camera: in millisecond
         time-step (/ 1000 fps)]
     (doall
      (for [idx (range nb-step)]
        (js/setTimeout
         (fn [] (update-camera! (nth list-camera idx)))
         (* time-step idx))))
     )))

(defn- target-dimension->zoom
  "From a target width and height for the camera, compute the zoom associated"
  [target-dimension]
  (let [camera-dimension [(@camera :width) (@camera :height)]
        zooms (map / camera-dimension target-dimension)
        weakest_zoom (apply min zooms)]
    weakest_zoom))

(defn home-evt []
  ;; TODO: animate the transition to the 'Home' standpoint
  (let [[cx cy] (state-read/graph-mid-pt)
        {:keys [width height]} (state-read/graph-width-height)
        border-factor 1.2
        target-dimension [(* border-factor width) (* border-factor height)]
        weakest_zoom (target-dimension->zoom target-dimension)

        new-camera
        (merge @camera {:cx cx :cy cy :zoom weakest_zoom})]
    ;; (update-camera! new-camera)
    (animate-camera-transition @camera new-camera 1 60)
    ))
