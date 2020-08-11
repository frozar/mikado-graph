(ns mikado.app
  (:require
   ;; [reagent.session :as session]
   ;; [reitit.frontend :as reitit]
   ;; [clerk.core :as clerk]
   ;; [accountant.core :as accountant]
   [bubble.camera :as camera]
   [bubble.core :as bubble]
   [bubble.event :as event]
   [bubble.state-gui :refer [event-queue]]
   [cljs.core.async :refer [put!]]
   [reagent.dom :as rdom]
   ))

;; TODO: put in place frontend router

(defn graphe-page []
  ;; Trigger the event loop listening
  (event/handle-event)
  (event/window-keydown-evt-on)
  (camera/window-resize-evt-on)
  (camera/mouse-wheel-evt-on)
  [bubble/svg-canvas])

(defn mount-root [component]
  (rdom/render [component] (.getElementById js/document "app")))

(defn ^:dev/before-load unlisten-global-event []
  ;; Stop the event loop listening
  (put! event-queue [:stop-listening])
  (event/window-keydown-evt-off)
  (camera/window-resize-evt-off)
  (camera/mouse-wheel-evt-off)
  )

(defn ^:dev/after-load reload! []
  (mount-root graphe-page))

(defn init! []
  (reload!))
