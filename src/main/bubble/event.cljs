(ns bubble.event
  (:require
   [bubble.state-write :as state-write]
   [cljs.core.async :refer [chan put! <! go-loop]]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

(def event-queue (chan))

(go-loop [[event & args] (<! event-queue)]
  (case event

    :create-bubble
    (let [[bubble-id new-cx new-cy] args]
      (state-write/create-bubble-and-link! bubble-id new-cx new-cy))

    :delete-bubble
    (let [[bubble-id] args]
      (state-write/delete-bubble-and-update-link! bubble-id))

    :delete-link
    (let [[src-id dst-id] args]
      (state-write/delete-link! src-id dst-id))

    :dragging
    (let [[id cx cy] args]
      (state-write/move-bubble! id cx cy)
      )

    :build-link-start
    (let [[id mouse-x mouse-y] args]
      (state-write/set-link-src! id)
      (state-write/set-mouse-position! mouse-x mouse-y)
      )

    :build-link-move
    (let [[mouse-x mouse-y] args]
      (state-write/set-mouse-position! mouse-x mouse-y))

    :build-link-end
    (let [[id] args]
      (state-write/building-link-end! id)
      (state-write/reset-build-link!)
      )

    :build-link-exit
    (state-write/reset-build-link!)

    :enable-edition
    (let [[id] args]
      (state-write/enable-edition! id))

    :disable-edition
    (let [[id] args]
      (state-write/disable-edition! id))

    :enable-show-button
    (let [[id] args]
      (state-write/enable-show-button! id))

    :disable-show-button
    (let [[id] args]
      (state-write/disable-show-button! id))

    :save-text
    (let [[id text] args]
      (state-write/save-text-bubble! id text))

    :toggle-done-status
    (let [[id] args]
      (state-write/toggle-done-status! id))

    :resize-bubble
    (let [[id rx ry] args]
      (state-write/resize-bubble! id rx ry))

    :toggle-rough-layout
    (state-write/toggle-rough-layout!)

    )
  (recur (<! event-queue)))

(defn window-keydown-evt [evt]
  (let [escape-key-code 27]
    (condp = (.-keyCode evt)
      escape-key-code
      (put! event-queue [:build-link-exit])
      nil
      )))

(defn window-keydown-evt-fn []
  (events/listen js/window EventType.KEYDOWN window-keydown-evt)
  )

(window-keydown-evt-fn) ;; auto-execution

(defn window-keypress-evt [evt]
  (let [t-key-code 116]
    (condp = (.-keyCode evt)
      t-key-code
      (put! event-queue [:toggle-rough-layout])

      nil
      )))

(defn window-keypress-evt-fn []
  (events/listen js/window EventType.KEYPRESS window-keypress-evt)
  )

(window-keypress-evt-fn) ;; auto-execution

(defn prevent-default
  ([] (prevent-default (fn [])))
  ([func]
   (fn [evt]
     (.preventDefault evt)
     (func))))
