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

(def interaction (atom nil))

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
      (reset! interaction "build-link")
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
      (reset! interaction nil)
      )

    :build-link-exit
    (do
      (state-write/reset-build-link!)
      (reset! interaction nil))

    :enable-edition
    (let [[id] args]
      (reset! interaction "edition")
      (state-write/enable-edition! id))

    :disable-edition
    (let [[id] args]
      (state-write/disable-edition! id)
      (reset! interaction nil))

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

(defn- window-keydown-evt
  "Configure the press to escape-key to exit interactive edition mode.
  Currently the only interaction is with the build-link action."
  [evt]
  (condp = (.-key evt)
    "Escape"
    (put! event-queue [:build-link-exit])

    "t"
    (when (not= @interaction "edition")
      (put! event-queue [:toggle-rough-layout]))

    nil
    ))

(defn window-keydown-evt-fn []
  (events/listen js/window EventType.KEYDOWN window-keydown-evt)
  )

;; TODO: define the expected behavior of window-resize-evt regarding the svg-canvas
(defn window-resize-evt []
  #_(reset! camera {:width (.-innerWidth js/window)
                  :height (.-innerHeight js/window)}))

(defn window-resize-evt-fn []
  (events/listen js/window EventType.RESIZE window-resize-evt)
  )

(defn prevent-default
  ([] (prevent-default (fn [])))
  ([func]
   (fn [evt]
     (.preventDefault evt)
     (func))))
