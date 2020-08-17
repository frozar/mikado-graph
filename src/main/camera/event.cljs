(ns camera.event
  (:require
   [camera.event.home :as home]
   [camera.event.pan :as pan]
   [camera.event.zoom :as zoom]
   [camera.state :as state]
   [cljs.core.async :refer [<! go-loop]]
   ))

(defn handle-event []
  (let [keep-listening? (atom true)
        panning-type :standard]
    (go-loop [[event & args] (<! state/event-queue)]
      (case event
        :pan-start
        (do
          (home/stop-background!)
          (let [[mouse-pos-win-px] args]
            (pan/panning panning-type :start mouse-pos-win-px)
            ))

        :pan-move
        (let [[mouse-pos-win-px] args]
          (pan/panning panning-type :move mouse-pos-win-px))

        :pan-stop
        (pan/panning panning-type :stop nil)

        :mouse-wheel
        (do
          (home/stop-background!)
          (let [[wheel-delta-y win-px-x win-px-y] args]
            (zoom/mouse-wheel wheel-delta-y win-px-x win-px-y)))

        :home
        (home/trigger-evt)

        :resize
        (let [[width height] args]
          (zoom/window-resize width height))

        :stop-listening
        (reset! keep-listening? false)
        )

      ;; If a :stop-listening message is received, exit.
      ;; Useful in the development mode for the hot reload
      (when @keep-listening?
        (recur (<! state/event-queue))))))
