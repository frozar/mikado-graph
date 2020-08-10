(ns bubble.simulation-to-bubble
  (:require
   [bubble.state-write :as state-write]
   [clojure.walk :as walk]
   [simulation.state :refer [current-simulation]]
   ))

(defn- js-node->cljs-node [nodes]
  (-> nodes
      js->clj
      walk/keywordize-keys))

(defn update-app-state-bubble-position [appstate]
  (when (not (nil? @current-simulation))
    (let [nodes (js-node->cljs-node (.nodes @current-simulation))
          nodes-good-shape
          (->> nodes
               (map
                (fn [{:keys [id x y]}]
                  [id {:cx x :cy y}]))
               (into {}))]
      (state-write/move-bubbles appstate nodes-good-shape))))

(defn update-app-state-bubble-position! []
  (when (not (nil? @current-simulation))
    (let [nodes (js-node->cljs-node (.nodes @current-simulation))
          nodes-good-shape
          (->> nodes
               (map
                (fn [{:keys [id x y]}]
                  [id {:cx x :cy y}]))
               (into {}))]
      (state-write/move-bubbles! nodes-good-shape))))
