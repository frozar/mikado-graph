(ns mikado.app
  (:require
   ;; [reagent.session :as session]
   ;; [reitit.frontend :as reitit]
   ;; [clerk.core :as clerk]
   ;; [accountant.core :as accountant]
   [bubble.core :as bubble]
   [bubble.event :as event]
   [bubble.gui.state :as gui-state]
   [camera.event :as camera-event]
   [camera.event.zoom :as camera-zoom]
   [camera.state :as camera-state]
   [cljs.core.async :refer [put!]]
   ))

(defn listen-global-event
  "Trigger the event loop listening for state and camera"
  []
  (event/handle-event)
  (event/window-keydown-evt-on)
  (camera-event/handle-event)
  (camera-zoom/window-resize-evt-on)
  (camera-zoom/mouse-wheel-evt-on))

(defn graphe-page []
  (listen-global-event)
  [bubble/svg-canvas])

(defn ^:dev/before-load unlisten-global-event
  "Stop the event loop listening for state and camera"
  []
  (put! gui-state/event-queue [:stop-listening])
  (event/window-keydown-evt-off)
  (put! camera-state/event-queue [:stop-listening])
  (camera-zoom/window-resize-evt-off)
  (camera-zoom/mouse-wheel-evt-off))
