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

(defn- camera->viewBox [camera]
  (let [width  (/ (:width camera)  (:zoom camera))
        height (/ (:height camera) (:zoom camera))
        min-x (- (:cx camera) (/ width 2.))
        min-y (- (:cy camera) (/ height 2.))]
    {:width width :height height :min-x min-x :min-y min-y}))

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

(declare svg-px->svg-user-coord)
(declare svg-user-coord->svg-px)

(defn- pt-dist
  [[x0 y0] [x1 y1]]
  (let [vx (- x1 x0)
        vy (- y1 y0)]
    (.sqrt js/Math (+ (* vx vx) (* vy vy)))))

(defn- mid-pt
  [pt0 pt1]
  (map (fn [v] (/ v 2))
       (map + pt0 pt1)))

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
        zoom-factor (/ 1 (- zoom0 zoom1 ))
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
(defn- camera-linear-interpolation-translation [src-camera dst-camera nb-step]
  (let [divisor (dec nb-step)
        {cx0 :cx cy0 :cy} src-camera
        {cx1 :cx cy1 :cy} dst-camera
        c0->c1 (map - [cx1 cy1] [cx0 cy0])
        inc-c0->c1 (map #(/ % divisor) c0->c1)
        ]
    (take
     nb-step
     (iterate
      (fn [camera]
        (let [camera-translated
              (-> camera
                  (update :cx + (nth inc-c0->c1 0))
                  (update :cy + (nth inc-c0->c1 1))
                  )]
          camera-translated)
        )
      src-camera))
    )
  )

(defn- camera-linear-interpolation-homothety [src-camera dst-camera nb-step]
  (let [divisor (dec nb-step)
        inc-zoom (/ (- (:zoom dst-camera) (:zoom src-camera)) divisor)

        fix-pt-svg-user (compute-fix-point-svg-user src-camera dst-camera)
        fix-pt-svg-px (svg-user-coord->svg-px src-camera fix-pt-svg-user)
        ]
    (take
     nb-step
     (iterate
      (fn [camera]
        (let [camera-zoomed
              (update camera :zoom + inc-zoom)

              next-camera
              (correct-camera-by-translation-fix-point-svg-px
               camera
               camera-zoomed
               fix-pt-svg-px)
              ]
          next-camera)
        )
      src-camera))))

(defn- camera-linear-interpolation
  [src-camera dst-camera nb-step]
  (cond
    ;; In case the 2 input cameras are just translation of each other
    (<
     (.abs js/Math (- (:zoom src-camera) (:zoom dst-camera)))
     10e-3)
    (camera-linear-interpolation-translation src-camera dst-camera nb-step)

    ;; In case a zoom/unzoom have been done
    :else
    (camera-linear-interpolation-homothety src-camera dst-camera nb-step)
    ))

(defn- compute-log-steps [dist nb-step]
  (for [idx (range 1 (inc nb-step))]
    (* dist
       (/ (.log js/Math idx) (.log js/Math nb-step)))))

(defn- sign [number]
  (if (pos? number) + -))

(defn- range-log [start end nb-step]
  (let [dist (.abs js/Math (- end start))
        log-steps (compute-log-steps dist nb-step)
        sign-operator (sign (- end start))
        signed-log-steps (map #(sign-operator %) log-steps)]
    (for [step signed-log-steps]
      (+ start step))
    ))

(comment
  (range-log 0 10 4)
  (range-log 0 -10 4)
  )

(defn- camera-logarithmic-interpolation-translation [src-camera dst-camera nb-step]
  (let [{cx0 :cx cy0 :cy} src-camera
        {cx1 :cx cy1 :cy} dst-camera

        cameras-center-x (range-log cx0 cx1 nb-step)
        cameras-center-y (range-log cy0 cy1 nb-step)

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

(comment
  (let [src-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
        dst-camera {:cx 800 :cy 600 :width 800 :height 600 :zoom 1}
        nb-step 3]
    (camera-logarithmic-interpolation-translation src-camera dst-camera nb-step)
    )
  )

(defn- camera-logarithmic-interpolation-homothety [src-camera dst-camera nb-step]
  (let [zoom0 (:zoom src-camera)
        zoom1 (:zoom dst-camera)

        cameras-zoom
        (->> (range-log zoom0 zoom1 nb-step)
             (map (fn [v] {:zoom v}))
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

(comment
  (let [src-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
        dst-camera {:cx 800 :cy 600 :width 800 :height 600 :zoom 2}
        nb-step 3]
    (camera-logarithmic-interpolation-homothety src-camera dst-camera nb-step)
    )
  )

(defn- camera-logarithmic-interpolation
  [src-camera dst-camera nb-step]
  (cond
    ;; In case the 2 input cameras are just translation of each other
    (<
     (.abs js/Math (- (:zoom src-camera) (:zoom dst-camera)))
     10e-3)
    (camera-logarithmic-interpolation-translation src-camera dst-camera nb-step)

    ;; In case a zoom/unzoom have been done
    :else
    (camera-logarithmic-interpolation-homothety src-camera dst-camera nb-step)
    ))

(defn- camera-interpolation
  [interpolation-type src-camera dst-camera nb-step]
  (if (< nb-step 2)
    []
    (cond
      ;; In case the 2 input cameras are the same
      (= src-camera dst-camera)
      (take nb-step (repeat src-camera))

      ;; In case a zoom/unzoom have been done
      :else
      (condp = interpolation-type
        :linear
        (camera-linear-interpolation src-camera dst-camera nb-step)

        :logarithmic
        (camera-logarithmic-interpolation src-camera dst-camera nb-step)
        ))))
;; END CAMERA INTERPOLATION

(defn- update-camera! [new-camera]
  (reset! camera new-camera))
;; END camera section

(defn camera->viewBox-str []
  (let [{width :width
         height :height
         min-x :min-x
         min-y :min-y}
        (camera->viewBox @camera)]
    (string/join " " [min-x min-y width height])))

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
         list-camera (camera-interpolation :logarithmic src-camera dst-camera nb-step)
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
  (let [[cx cy] (state-read/graph-mid-pt)
        {:keys [width height]} (state-read/graph-width-height)
        border-factor 1.2
        target-dimension [(* border-factor width) (* border-factor height)]
        weakest_zoom (target-dimension->zoom target-dimension)

        target-camera (merge @camera {:cx cx :cy cy :zoom weakest_zoom})
        animation-duration 0.8
        animation-fps 60]
    (animate-camera-transition @camera target-camera animation-duration animation-fps)
    ))
