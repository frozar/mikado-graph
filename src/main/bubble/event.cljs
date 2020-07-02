(ns bubble.event
  (:require
   [bubble.camera :as camera]
   [bubble.state-write :as state-write]
   [cljs.core.async :refer [chan put! <! go-loop]]
   [goog.events :as events]
   [simulation.core]
   )
  (:import
   [goog.events EventType]
   ))

(def event-queue (chan))

(def interaction (atom nil))
(def simulation  (atom true))

(go-loop [[event & args] (<! event-queue)]
  (case event

    :create-bubble
    (let [[bubble-id new-cx new-cy] args]
      (if @simulation
        (let [new-state (state-write/simulation-create-bubble-and-link bubble-id)]
          (simulation.core/launch-simulation new-state event-queue))
        (state-write/create-bubble-and-link! bubble-id new-cx new-cy)))

    :delete-bubble
    (let [[bubble-id] args]
      (state-write/delete-bubble-and-update-link! bubble-id))

    :delete-link
    (let [[src-id dst-id] args]
      (state-write/delete-link! src-id dst-id))

    :simulation-move
    (let [[nodes] args
          nodes-good-shape
          (->> nodes
               (map
                (fn [{:keys [id x y]}]
                  [id {:cx x :cy y}]))
               (into {}))]
      ;; (.log js/console "EVENT nodes"
      ;;       nodes-good-shape)
      (state-write/move-bubbles! nodes-good-shape))

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

    "Home"
    (when (not= @interaction "edition")
      (put! camera/event-queue [:home]))

    "s"
    (when (not= @interaction "edition")
      (swap! simulation not))

    nil
    ))

(defn window-keydown-evt-fn []
  (events/listen js/window EventType.KEYDOWN window-keydown-evt)
  )

(defn window-keydown-evt-off []
  (events/unlisten js/window EventType.KEYDOWN window-keydown-evt)
  )

(defn prevent-default
  ([] (prevent-default (fn [])))
  ([func]
   (fn [evt]
     (.preventDefault evt)
     (func))))
