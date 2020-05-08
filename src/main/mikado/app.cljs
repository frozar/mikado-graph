(ns mikado.app
  (:require
   ;; [reagent.session :as session]
   ;; [reitit.frontend :as reitit]
   ;; [clerk.core :as clerk]
   ;; [accountant.core :as accountant]
   [bubble.camera :as camera]
   [bubble.core :as bubble]
   [bubble.event :as event]
   [reagent.dom :as rdom]
   ))

;; TODO: put in place frontend router

(defn graphe-page []
  (event/window-keydown-evt-fn)
  (camera/window-resize-evt-fn)
  (camera/mouse-wheel-evt-fn)
  [bubble/svg-canvas])

(defn current-page []
  [:h1 "Hello"]
  )

(defn mount-root [component]
  (rdom/render [component] (.getElementById js/document "app")))

(defn ^:dev/before-load unlisten-global-event []
  (event/window-keydown-evt-off)
  (camera/window-resize-evt-off)
  (camera/mouse-wheel-evt-off)
  )

(defn ^:dev/after-load reload! []
  (mount-root graphe-page)
  )

(defn init! []
  (reload!)
  )
