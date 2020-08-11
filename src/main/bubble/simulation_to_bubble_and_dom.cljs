(ns bubble.simulation-to-bubble-and-dom
  (:require
   [bubble.core :as bubble]
   [bubble.state-write :as state-write]
   [bubble.simulation-to-bubble :refer [js-node->cljs-node]]
   [reagent.dom :as rdom]
   [simulation.state :refer [current-simulation]]
   ))

(defn update-app-state-bubble-position-and-remount! []
  (when-not (nil? @current-simulation)
    (let [nodes (js-node->cljs-node (.nodes @current-simulation))
          nodes-good-shape
          (->> nodes
               (map
                (fn [{:keys [id x y]}]
                  [id {:cx x :cy y}]))
               (into {}))]
      (state-write/move-bubbles! nodes-good-shape)
      (rdom/unmount-component-at-node (.getElementById js/document "app"))
      (rdom/render [bubble/svg-canvas] (.getElementById js/document "app"))
      )))
