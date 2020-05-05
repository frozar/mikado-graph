(ns bubble.camera
  (:require
   [clojure.string :as string]
   [reagent.core :as reagent]
   ))

(def camera
  (let [width (.-innerWidth js/window)
        height (.-innerHeight js/window)]
    (reagent/atom
     {:cx (/ width 2)
      :cy (/ height 2)
      :width width
      :height height
      :zoom 1.})))

(defn camera->viewBox []
  (let [width (:width @camera)
        height (:height @camera)
        min-x (- (:cx @camera) (/ width 2))
        min-y (- (:cy @camera) (/ height 2))]
    (string/join " " [min-x min-y width height])))
