(ns mikado.app
  (:require
   ;; [reagent.session :as session]
   ;; [reitit.frontend :as reitit]
   ;; [clerk.core :as clerk]
   ;; [accountant.core :as accountant]
   [bubble.core :as bubble]
   [bubble.event :as event]
   [bubble.gui.state :as gui-state]
   [camera.core :as camera-core]
   [camera.state :as camera-state]
   [cljs.core.async :refer [put!]]
   [reagent.dom :as rdom]
   ))

;; TODO: put in place frontend router

(defn graphe-page []
  ;; Trigger the event loop listening
  (event/handle-event)
  (event/window-keydown-evt-on)
  (camera-core/handle-event)
  (camera-core/window-resize-evt-on)
  (camera-core/mouse-wheel-evt-on)
  [bubble/svg-canvas])

(defn mount-root [component]
  (rdom/render [component] (.getElementById js/document "app")))

(defn ^:dev/before-load unlisten-global-event []
  ;; Stop the event loop listening for app state modification
  (put! gui-state/event-queue [:stop-listening])
  (event/window-keydown-evt-off)
  ;; Stop the event loop listening for camera modification
  (put! camera-state/event-queue [:stop-listening])
  (camera-core/window-resize-evt-off)
  (camera-core/mouse-wheel-evt-off)
  )

(defn ^:dev/after-load reload! []
  (mount-root graphe-page))

(defn init! []
  (reload!))
