(ns bubble.camera
  (:require
   [bubble.coordinate :as coord]
   [clojure.string :as string]
   [reagent.core :as reagent]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

(defonce camera
  (let [width (.-innerWidth js/window)
        height (.-innerHeight js/window)]
    (reagent/atom
     {:cx (/ width 2.)
      :cy (/ height 2.)
      :width width
      :height height
      :zoom 1.})))

(defn camera->viewBox []
  (let [width  (/ (:width @camera)  (:zoom @camera))
        height (/ (:height @camera) (:zoom @camera))
        min-x (- (:cx @camera) (/ width 2.))
        min-y (- (:cy @camera) (/ height 2.))]
    (string/join " " [min-x min-y width height])))

(defn svg-px->svg-user-coord
  "From svg pixel position, convert to svg user position."
  [camera svg-px]
  (let [svg-scaled (map / svg-px [(:zoom camera) (:zoom camera)])

        mid-plan-px (map / [(:width camera) (:height camera)] [2 2])
        mid-plan-scaled (map / mid-plan-px [(:zoom camera) (:zoom camera)])

        pt-update-origin (map + svg-scaled [(:cx camera) (:cy camera)])
        pt-user-coord (map - pt-update-origin mid-plan-scaled)]
    pt-user-coord))

(defn win-px->svg-user-coord
  "From window pixel position, convert to svg user position."
  [camera win-px]
  (let [svg-px (coord/win-px->svg-px win-px)

        pt-user-coord (svg-px->svg-user-coord camera svg-px)]
    pt-user-coord
    ))

(defn apply-zoom
  "Invariant: the svg-user coordinates associated with the input svg-px must match
  after the camera update"
  [camera scale pt-svg-px]
  (let [camera-zoomed (update camera :zoom * scale)
        pt-before-zoom-svg-user (svg-px->svg-user-coord camera pt-svg-px)
        pt-after-zoom-svg-user  (svg-px->svg-user-coord camera-zoomed pt-svg-px)
        vec-trans-svg-user (map - pt-after-zoom-svg-user pt-before-zoom-svg-user)
        [cx cy] [(:cx camera-zoomed) (:cy camera-zoomed)]
        [new-cx new-cy] (map - [cx cy] vec-trans-svg-user)
        camera-zoomed-n-translated (merge camera-zoomed {:cx new-cx :cy new-cy})
        ]
    camera-zoomed-n-translated
    ))

(defn mouse-wheel-evt [evt]
  (let [reduction-speed-factor (if (.-shiftKey evt) 10 5)
        scale (.pow js/Math 1.005 (/ (..  evt -event_ -wheelDeltaY) reduction-speed-factor))
        win-px [(.-clientX evt) (.-clientY evt)]]
    (swap! camera
           apply-zoom scale
           (coord/win-px->svg-px win-px))
    ))

(defn mouse-scroll-evt-fn []
  (events/listen js/window EventType.WHEEL mouse-wheel-evt))
