(ns bubble.coordinate
  (:require
   [reagent.core :as reagent]
   ))

(defonce svg-origin (reagent/atom nil))

(defn init-svg-origin! [svg-origin-x svg-origin-y]
  (reset! svg-origin [svg-origin-x svg-origin-y])
  )

(defn get-svg-coord
  [x y]
  (map - [x y] @svg-origin))
