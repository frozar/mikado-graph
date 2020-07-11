(ns bubble.event
  (:require
   [bubble.camera :as camera]
   [bubble.state-read :as state-read]
   [bubble.state-write :as state-write]
   [cljs.core.async :refer [chan put! <! go-loop]]
   [goog.events :as events]
   [simulation.core]
   )
  (:import
   [goog.events EventType]
   ))

(def event-queue (chan))

;; TODO: try to get ride of this variable
(def interaction (atom nil))

;; Store the settings if simulation is enable or not
(def simulation? (atom true))

(def print-debug? false)

(go-loop [[event & args] (<! event-queue)]
  (when print-debug?
    (.debug js/console "event " event)
    (.debug js/console "args " args))
  (case event

    :create-bubble
    (let [[bubble-id new-cx new-cy] args]
      (if @simulation?
        (let [new-state (state-write/simulation-create-bubble-and-link bubble-id)]
          (simulation.core/launch-simulation! new-state event-queue))
        (state-write/create-bubble-and-link! bubble-id new-cx new-cy)))

    :delete-bubble
    (let [[bubble-id] args
          new-state (state-write/delete-bubble-and-update-link! bubble-id)]
      (when @simulation?
        (simulation.core/launch-simulation! new-state event-queue)))

    :delete-link
    (let [[src-id dst-id] args
          new-state (state-write/delete-link! src-id dst-id)]
      (when @simulation?
        (simulation.core/launch-simulation! new-state event-queue)))

    :simulation-move
    (let [[nodes] args
          nodes-good-shape
          (->> nodes
               (map
                (fn [{:keys [id x y]}]
                  [id {:cx x :cy y}]))
               (into {}))]
      (state-write/move-bubbles! nodes-good-shape))

    :dragging
    (let [[id cx cy] args]
      (if @simulation?
        (simulation.core/simulation-drag! (state-read/get-state) id cx cy event-queue)
        (state-write/move-bubble! id cx cy)))

    :dragging-end
    (let [[id] args]
      (when @simulation?
        (simulation.core/simulation-drag-end! id)))

    :build-link-start
    (let [[id mouse-x mouse-y] args]
      (reset! interaction "build-link")
      (state-write/set-link-src! id)
      (state-write/set-mouse-position! mouse-x mouse-y))

    :build-link-move
    (let [[mouse-x mouse-y] args]
      (state-write/set-mouse-position! mouse-x mouse-y))

    :build-link-end
    (let [[id] args
          new-state (state-write/building-link-end! id)]
      (when @simulation?
        (simulation.core/launch-simulation! new-state event-queue))
      (state-write/reset-build-link!)
      (reset! interaction nil))

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
  (when print-debug?
    (.debug js/console "appstate " (state-read/get-state)))
  (recur (<! event-queue)))

(defn- window-keydown-evt
  "Configure the press to escape-key to exit interactive edition mode.
  Currently the only interaction is with the build-link action."
  [evt]
  (condp = (.-key evt)
    "t"
    (when (not= @interaction "edition")
      (put! event-queue [:toggle-rough-layout]))

    "Home"
    (when (not= @interaction "edition")
      (put! camera/event-queue [:home]))

    "s"
    (when (not= @interaction "edition")
      (.debug js/console "@simulation? " (not @simulation?))
      (swap! simulation? not))

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
