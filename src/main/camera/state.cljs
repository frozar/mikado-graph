(ns camera.state
  (:require
   [cljs.core.async :refer [chan]]
   [reagent.core :as reagent]
   ))

(def event-queue (chan))

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
