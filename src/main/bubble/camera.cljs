(ns bubble.camera
  (:require
   [bubble.coordinate :as coord]
   [bubble.state-read :as state-read]
   [clojure.string :as string]
   [reagent.core :as reagent]
   [goog.events :as events]
   [debux.cs.core :refer-macros [clog clogn dbg dbgn break]]
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

(defn state []
  (dissoc @camera :zoom))

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

(defn camera-viewBox-area [camera]
  (let [{width :width
         height :height} (camera->viewBox camera)]
    (* width height)))

(declare svg-px->svg-user-coord)
(declare svg-user-coord->svg-px)

(defn- get-top-left-corner-svg-user [camera]
  (let [{:keys [min-x min-y]} (camera->viewBox camera)
        ;; vector in svg-user space: origin = pt (0, 0) in user space
        top-left-corner-svg-user [min-x min-y]]
    top-left-corner-svg-user))

(defn- compute-fix-point-svg-user
  [camera0 camera1]
  (let [;; deduce from svg-user-coord->svg-px
        ;; z0 * svg-scaled0 = z1 * svg-scaled1
        ;; z0 * (pt-svg-user - top-left0) = z1 * (pt-svg-user - top-left1)
        ;; (z0 - z1) * pt-svg-user = z0 * top-left0 - z1 * top-left1
        ;; pt-svg-user = (1 / (z0 - z1)) (z0 * top-left0 - z1 * top-left1)
        zoom0 (:zoom camera0)
        zoom1 (:zoom camera1)
        top-left-corner-svg-user0 (get-top-left-corner-svg-user camera0)
        top-left-corner-svg-user1 (get-top-left-corner-svg-user camera1)
        zoom-factor (/ 1 (- zoom0 zoom1))
        tmp-pt (map -
                    (map #(* zoom0 %) top-left-corner-svg-user0)
                    (map #(* zoom1 %) top-left-corner-svg-user1))
        fix-pt-svg-user (map #(* zoom-factor %) tmp-pt)
        ]
    fix-pt-svg-user
    ))

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

;; BEGIN CAMERA INTERPOLATION
(defn- compute-linear-steps [dist nb-step]
  (for [idx (range 0 nb-step)]
    (* dist
       (/ idx (dec nb-step)))))

(defn- compute-log-steps [dist nb-step]
  (for [idx (map inc (compute-linear-steps 9 nb-step))]
    (* dist
       (/ (.log js/Math idx) (.log js/Math 10)))))

(defn- sign [number]
  (if (pos? number) + -))

(defn- higher-range
  "interpolation-type: [:linear|:log]"
  [start end nb-step interpolation-type]
  (let [dist (.abs js/Math (- end start))
        compute-steps-fn
        (condp = interpolation-type
          :linear compute-linear-steps
          :log    compute-log-steps
          )
        log-steps (compute-steps-fn dist nb-step)
        sign-operator (sign (- end start))
        signed-log-steps (map #(sign-operator %) log-steps)]
    (for [step signed-log-steps]
      (+ start step))
    ))

(defn- range-math
  ([start end nb-step] (range-math start end nb-step :linear))
  ([start end nb-step interpolation-type]
   (condp = interpolation-type
     :linear
     (higher-range start end nb-step :linear)

     :log
     (higher-range start end nb-step :log)
     ))
  )

(defn- camera-interpolation-translation [src-camera dst-camera nb-step interpolation-type]
  (let [{cx0 :cx cy0 :cy} src-camera
        {cx1 :cx cy1 :cy} dst-camera

        cameras-center-x (range-math cx0 cx1 nb-step interpolation-type)
        cameras-center-y (range-math cy0 cy1 nb-step interpolation-type)

        cameras-center
        (->> (interleave cameras-center-x cameras-center-y)
             (partition 2)
             (map (fn [[cx cy]] {:cx cx :cy cy}))
             )
        ]
    (for [center cameras-center]
      (merge src-camera center))
    )
  )

(defn- camera-interpolation-homothety [src-camera dst-camera nb-step interpolation-type]
  (let [zoom0 (:zoom src-camera)
        zoom1 (:zoom dst-camera)

        cameras-zoom
        (->> (range-math zoom0 zoom1 nb-step interpolation-type)
             (map (fn [zoom-value] {:zoom zoom-value}))
             (drop 1))

        fix-pt-svg-user (compute-fix-point-svg-user src-camera dst-camera)
        fix-pt-svg-px   (svg-user-coord->svg-px src-camera fix-pt-svg-user)

        list-camera-zoomed
        (take
         nb-step
         (iterate
          (fn [[camera idx]]
            (let [current-zoom (nth cameras-zoom idx)
                  camera-zoomed (merge camera current-zoom)

                  next-camera
                  (correct-camera-by-translation-fix-point-svg-px
                   camera camera-zoomed fix-pt-svg-px)]
              [next-camera (inc idx)]))
          [src-camera 0]))
        ]
    (map first list-camera-zoomed)))

(defn- camera-interpolation*
  [src-camera dst-camera nb-step interpolation-type]
  (let [dist-zoom (.abs js/Math (- (:zoom src-camera) (:zoom dst-camera)))]
    (cond
      ;; In case the 2 input cameras are just translation of each other
      (< dist-zoom 10e-3)
      (camera-interpolation-translation src-camera dst-camera nb-step interpolation-type)

      ;; In case a zoom/unzoom have been done
      :else
      (camera-interpolation-homothety src-camera dst-camera nb-step interpolation-type)
      )))

(defn- camera-interpolation
  [src-camera dst-camera nb-step interpolation-type]
  (if (< nb-step 2)
    []
    (cond
      ;; In case the 2 input cameras are the same
      (= src-camera dst-camera)
      (take nb-step (repeat src-camera))

      ;; In case a zoom/unzoom have been done
      :else
      (camera-interpolation* src-camera dst-camera nb-step interpolation-type)
      )))
;; END CAMERA INTERPOLATION
(defn update-camera [hashmap]
  (merge @camera hashmap))

(defn scale-dist [dist]
  (let [zoom (:zoom @camera)]
    (/ dist zoom)))

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
  (and (< 0.05 (area-ratio-min-bubble<->viewBox camera))
       (< (area-ratio-min-bubble<->viewBox camera) 50)))

(defn- bbox-area [{left :left right :right top :top bottom :bottom}]
  (let [width (- right left)
        height (- bottom top)]
    (if (or (neg? width) (neg? height))
      0
      (* width height))))

(defn in-pan-limit?
  "Check if the graph is still enough in the displayed viewBox. Typically,
  if less than 30% of the graph is displayed, return false."
  ([] (in-pan-limit? 30))
  ([minimal-ratio] (in-pan-limit? @camera minimal-ratio))
  ([camera minimal-ratio]
   (let [{width :width height :height
          view-left :min-x view-top :min-y} (camera->viewBox camera)
         [view-right view-bottom] (map + [view-left view-top] [width height])
         {graph-left :left graph-right :right
          graph-top :top graph-bottom :bottom} (state-read/graph-bbox)

         intersection-bbox
         {:left (max view-left graph-left)
          :right (min view-right graph-right)
          :top (max view-top graph-top)
          :bottom (min view-bottom graph-bottom)}

         intersection-bbox-area (bbox-area intersection-bbox)

         area-ratio (* 100 (/ intersection-bbox-area (state-read/graph-bbox-area)))]
     (< minimal-ratio area-ratio)
     )))

(defn set-camera! [new-camera]
  ;; TODO: smooth the transition
  (let [is-in-zoom-limit (in-zoom-limit? new-camera)]
    (when (and is-in-zoom-limit
               #_is-in-pan-limit)
      (reset! camera new-camera))))
;; END camera section

(defn svg-px->svg-user-coord
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

(defn svg-user-coord->svg-px
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

(defn win-px->svg-user-coord
  "From window pixel position, convert to svg user position."
  ([pt-win-px] (win-px->svg-user-coord @camera pt-win-px))
  ([camera pt-win-px]
   (let [svg-px (coord/win-px->svg-px pt-win-px)

         pt-user-coord (svg-px->svg-user-coord camera svg-px)]
     pt-user-coord
     )))

(defn- mouse-wheel-evt [evt]
  (let [reduction-speed-factor (if (.-shiftKey evt) 10 5)
        scale (.pow js/Math 1.005 (/ (..  evt -event_ -wheelDeltaY) reduction-speed-factor))
        win-px [(.-clientX evt) (.-clientY evt)]
        new-camera (apply-zoom @camera scale (coord/win-px->svg-px win-px))]
    (set-camera! new-camera)))

(defn mouse-wheel-evt-fn []
  (events/listen js/window EventType.WHEEL mouse-wheel-evt))

(defn mouse-wheel-evt-off []
  (events/unlisten js/window EventType.WHEEL mouse-wheel-evt))

(defn- window-resize-evt []
  (let [new-camera
        (apply-resize @camera (.-innerWidth js/window) (.-innerHeight js/window))]
    (set-camera! new-camera)))

(defn window-resize-evt-fn []
  (events/listen js/window EventType.RESIZE window-resize-evt))

(defn window-resize-evt-off []
  (events/unlisten js/window EventType.RESIZE window-resize-evt))

(defn animate-camera-transition
  "duration: in second"
  ([dst-camera duration]
   (animate-camera-transition @camera dst-camera duration 60))
  ([src-camera dst-camera duration]
   (animate-camera-transition src-camera dst-camera duration 60))
  ([src-camera dst-camera duration fps]
   (let [nb-step (* duration fps)
         list-camera (camera-interpolation src-camera dst-camera nb-step :log)
         ;; the time between camera (time-step): in millisecond
         time-step (/ 1000 fps)]
     (doall
      (for [idx (range nb-step)]
        (js/setTimeout
         (fn [] (set-camera! (nth list-camera idx)))
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
  (let [[cx cy] (state-read/graph-mid-pt)
        {:keys [width height]} (state-read/graph-width-height)
        border-factor 1.3
        target-dimension [(* border-factor width) (* border-factor height)]
        weakest_zoom (target-dimension->zoom target-dimension)

        target-camera (merge @camera {:cx cx :cy cy :zoom weakest_zoom})
        animation-duration 0.8
        animation-fps 60]
    (animate-camera-transition @camera target-camera animation-duration animation-fps)
    ))
