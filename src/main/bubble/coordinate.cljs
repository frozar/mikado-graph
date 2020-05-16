(ns bubble.coordinate
  (:require
   [reagent.core :as reagent]
   ))

(defonce svg-origin (reagent/atom nil))

(defn init-svg-origin! [svg-origin-x-px svg-origin-y-px]
  (reset! svg-origin [svg-origin-x-px svg-origin-y-px])
  )

(defn win-px->svg-px
  "Change the coordinate system from the window frame to the svg-canvas frame.
  The coordinate are still in pixel."
  [pt-in-window-frame]
  (map - pt-in-window-frame @svg-origin))
