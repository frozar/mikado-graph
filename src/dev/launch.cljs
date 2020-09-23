(ns launch
  (:require
   [mikado.app]
   [reagent.dom :as rdom]
   ))

(defn mount-root [component]
  (rdom/render [component] (.getElementById js/document "app")))

(defn ^:dev/after-load reload! []
  (mount-root mikado.app/graphe-page))

(defn init! []
  (reload!))
