(ns camera.event
  (:require
   [camera.core :as core]
   [camera.state :as state]
   [cljs.core.async :refer [<! go-loop]]
   ;; [goog.events :as events]
   )
  #_(:import
   [goog.events EventType]
   ))

(defn handle-event []
  (let [keep-listening? (atom true)
        panning-type :standard]
    (go-loop [[event & args] (<! state/event-queue)]
      (case event
        :pan-start
        (do
          (core/home-evt-stop-background!)
          (let [[mouse-pos-win-px] args]
            (core/panning panning-type :start mouse-pos-win-px)
            ))

        :pan-move
        (let [[mouse-pos-win-px] args]
          (core/panning panning-type :move mouse-pos-win-px))

        :pan-stop
        (core/panning panning-type :stop nil)

        :mouse-wheel
        (do
          (core/home-evt-stop-background!)
          (let [[wheel-delta-y win-px-x win-px-y] args]
            (core/mouse-wheel wheel-delta-y win-px-x win-px-y)))

        :home
        (core/home-evt)

        :resize
        (let [[width height] args]
          (core/window-resize width height))

        :stop-listening
        (reset! keep-listening? false)
        )

      ;; If a :stop-listening message is received, exit.
      ;; Useful in the development mode for the hot reload
      (when @keep-listening?
        (recur (<! state/event-queue))))))
