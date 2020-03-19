(ns bubble.event
  (:require [bubble.state :as state]
            [cljs.core.async :refer [chan put! <! go-loop]]
            ))

(def event-queue (chan))

(go-loop [ [event & args] (<! event-queue)]
  (case event

    :create-bubble
    (let [ [bubble-id new-center-x new-center-y] args ]
      (state/create-bubble bubble-id new-center-x new-center-y))

    :delete-bubble
    (let [ [bubble-id] args ]
      (state/delete-bubble bubble-id))

    :dragging
    (let [ [id cx cy] args ]
      ((state/move-bubble id) cx cy)
      )
    )
  (recur (<! event-queue)))
