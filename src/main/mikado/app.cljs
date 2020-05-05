(ns mikado.app
  (:require
   [bubble.camera :as camera]
   [bubble.core :as bubble]
   [bubble.event :as event]
   [reagent.dom :as rdom]
   ))

;; TODO: put in place frontend router

(defn graphe-page []
  [bubble/svg-canvas])

(defn current-page []
  [:h1 "Hello"]
  )

(defn mount-root [component]
  (rdom/render [component] (.getElementById js/document "app"))
  (event/window-keydown-evt-fn)
  (event/window-resize-evt-fn)
  (camera/mouse-scroll-evt-fn)
  )

(defn ^:dev/after-load reload! []
  (mount-root graphe-page)
  )

(defn init! []
  (reload!)
  )
