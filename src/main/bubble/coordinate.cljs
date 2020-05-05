(ns bubble.coordinate
  (:require
   [bubble.camera :as camera]
   [reagent.core :as reagent]
   ))

(defonce svg-origin (reagent/atom nil))

(defn init-svg-origin! [svg-origin-x svg-origin-y]
  (reset! svg-origin [svg-origin-x svg-origin-y])
  )

(defn window->svg-canvas-px
  "Change the coordinate system from the window frame to the svg-canvas frame.
  The coordinate are still in pixel."
  [pt-in-window-frame]
  (map - pt-in-window-frame @svg-origin))

(defn svg-canvas-px->svg-canvas-user
  "Change the coordinate system from the window frame to the svg-canvas frame.
  The coordinate are still in pixel."
  [[x-px y-px]]
  (let [cx-to-x (- x-px (:cx @camera/camera))
        cy-to-y (- y-px (:cy @camera/camera))]
    [(/ cx-to-x (:zoom @camera/camera))
     (/ cy-to-y (:zoom @camera/camera))]))

(defn get-svg-coord
  [x-px y-px]
  (-> [x-px y-px]
      window->svg-canvas-px
      #_svg-canvas-px->svg-canvas-user))
