(ns bubble.camera
  (:require
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
     {:cx (/ width 2)
      :cy (/ height 2)
      :width width
      :height height
      :zoom 1.})))

(defn camera->viewBox []
  (let [width (/ (:width @camera) (:zoom @camera))
        height (/ (:height @camera)  (:zoom @camera))
        min-x (- (:cx @camera) (/ width 2))
        min-y (- (:cy @camera) (/ height 2))]
    (string/join " " [min-x min-y width height])))

(defn mouse-wheel-evt [evt]
  (let [scale (.pow js/Math 1.005 (/ (..  evt -event_ -wheelDeltaY) 10))]
    (swap! camera (fn [cam] (update cam :zoom #(* % scale))))
    )
  )

(defn mouse-scroll-evt-fn []
  (events/listen js/window EventType.WHEEL mouse-wheel-evt))
