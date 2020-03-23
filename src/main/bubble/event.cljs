(ns bubble.event
  (:require [goog.events :as events]
            [bubble.state :as state]
            [cljs.core.async :refer [chan put! <! go-loop]]
            )
  (:import [goog.events EventType]
           ))

(def event-queue (chan))

(go-loop [[event & args] (<! event-queue)]
  (case event

    :create-bubble
    (let [[bubble-id new-center-x new-center-y] args]
      (state/create-bubble-and-link! bubble-id new-center-x new-center-y))

    :delete-bubble
    (let [[bubble-id] args]
      (state/delete-bubble-and-update-link! bubble-id))

    :dragging
    (let [[id cx cy] args]
      (state/move-bubble! id cx cy)
      )

    :build-link-start
    (let [[id] args]
      (state/set-link-src id)
      )

    :build-link-move
    (let [[mouse-x mouse-y] args]
      (state/set-mouse-position mouse-x mouse-y))

    :build-link-end
    (let [[id] args]
      (state/building-link-end id)
      (state/reset-build-link)
      )

    :build-link-exit
    (state/reset-build-link)

    :enable-edition
    (let [[id] args]
      (state/enable-edition! id))

    :disable-edition
    (let [[id] args]
      (state/disable-edition! id))

    )
  (recur (<! event-queue)))

(defn window-keydown-evt [evt]
  (let [escape-key-code 27]
    (if (= escape-key-code (.-keyCode evt))
      (put! event-queue [:build-link-exit]))))

(defn window-keydown-evt-fn []
  (events/listen js/window EventType.KEYDOWN window-keydown-evt)
  )

(window-keydown-evt-fn) ;; auto-execution
