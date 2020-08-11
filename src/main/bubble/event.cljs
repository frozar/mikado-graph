(ns bubble.event
  (:require
   [bubble.camera :as camera]
   [bubble.core :as bubble]
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.simulation-to-bubble :as simulation-to-bubble]
   [bubble.state-gui :as state-gui]
   [bubble.state-read :as state-read]
   [bubble.state-write :as state-write]
   [cljs.core.async :refer [put! <! go-loop]]
   [goog.events :as events]
   [reagent.dom :as rdom]
   [simulation.core]
   )
  (:import
   [goog.events EventType]
   ))

;; TODO: try to get ride of this variable
(def interaction (atom nil))

(def print-debug? false)

(defn handle-event []
  (let [keep-listening? (atom true)]
    (go-loop [[event & args] (<! state-gui/event-queue)]
      (when print-debug?
        (.debug js/console "event " event)
        (.debug js/console "args " args))
      (case event

        :create-bubble
        (let [[bubble-id new-cx new-cy] args]
          (if @state-gui/simulation?
            (do
              (simulation-to-bubble/update-app-state-bubble-position!)
              (let [new-state (state-write/simulation-create-bubble-and-link! bubble-id)]
                (simulation.core/launch-simulation! new-state state-gui/event-queue)))
            (state-write/create-bubble-and-link! bubble-id new-cx new-cy)))

        :delete-bubble
        (let [[bubble-id] args]
          (if @state-gui/simulation?
            (do
              (simulation-to-bubble/update-app-state-bubble-position!)
              (let [new-state (state-write/delete-bubble-and-update-link! bubble-id)]
                (simulation.core/launch-simulation! new-state state-gui/event-queue)))
            (state-write/delete-bubble-and-update-link! bubble-id)))

        :delete-link
        (let [[src-id dst-id] args
              new-state (state-write/delete-link! src-id dst-id)]
          (when (and @state-gui/simulation?
                     (state-read/is-connected? (state-read/get-state) ROOT-BUBBLE-ID src-id)
                     (state-read/is-connected? (state-read/get-state) ROOT-BUBBLE-ID dst-id))
            (simulation.core/launch-simulation! new-state state-gui/event-queue)))

        :simulation-move
        (let [[nodes] args
              nodes-good-shape
              (->> nodes
                   (map
                    (fn [{:keys [id x y]}]
                      [id {:cx x :cy y}]))
                   (into {}))]
          (state-write/move-bubbles! nodes-good-shape)
          (rdom/unmount-component-at-node (.getElementById js/document "app"))
          (rdom/render [bubble/svg-canvas] (.getElementById js/document "app"))
          (comment
            (-> (state-read/get-bubbles) keys count)
            (state-write/create-random-bubble-and-link! 50)
            )
          )

        :dragging
        (let [[id cx cy] args
              connected-graph (state-read/connected-graph (state-read/get-state) ROOT-BUBBLE-ID)
              nb-nodes (-> connected-graph state-read/get-bubbles count)
              is-connected?
              (and (not (nil? (some #{ROOT-BUBBLE-ID} (-> connected-graph :bubbles keys))))
                   (not (nil? (some #{id} (-> connected-graph :bubbles keys)))))]
          ;; (js/console.log "dragging @state-gui/simulation? " @state-gui/simulation?)
          ;; (js/console.log "id cx cy " id cx cy)
          ;; (js/console.log "connected-graph " connected-graph)
          ;; (js/console.log "is-connected? " is-connected?)
          (if (and @state-gui/simulation?
                   ;; (state-read/is-connected? (state-read/get-state) ROOT-BUBBLE-ID id)
                   is-connected?
                   (< 1 nb-nodes))
            (do
              ;; (js/console.log "event drag! id " cx cy)
              ;; (simulation.core/update-app-state-bubble-position state-gui/event-queue)
              (simulation.core/simulation-drag! (state-read/get-state) id cx cy state-gui/event-queue))
            (state-write/move-bubble! id cx cy)
            ))
        #_(let [[id cx cy] args]
            (state-write/move-bubble! id cx cy))
        ;; nil

        :dragging-end
        (let [[id] args
              connected-graph (state-read/connected-graph (state-read/get-state) ROOT-BUBBLE-ID)
              nb-nodes (-> connected-graph state-read/get-bubbles count)]
          ;; (js/console.log "dragging-end @state-gui/simulation? " @state-gui/simulation?)
          (when (and @state-gui/simulation?
                     (< 1 nb-nodes))
            (simulation.core/simulation-drag-end! id)))
        ;; nil

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
          (when (and @state-gui/simulation?
                     (state-read/is-connected? (state-read/get-state) ROOT-BUBBLE-ID id))
            (simulation.core/launch-simulation! new-state state-gui/event-queue))
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

        :stop-listening
        (reset! keep-listening? false)
        )

      (when print-debug?
        (.debug js/console "appstate " (state-read/get-state)))
      ;; If a :stop-listening message is received, exit.
      ;; Useful in the development mode for the hot reload
      (when @keep-listening?
        (recur (<! state-gui/event-queue))))))

(defn- window-keydown-evt
  "Configure the press to escape-key to exit interactive edition mode.
  Currently the only interaction is with the build-link action."
  [evt]
  (condp = (.-key evt)
    "t"
    (when (not= @interaction "edition")
      (put! state-gui/event-queue [:toggle-rough-layout]))

    "Home"
    (when (not= @interaction "edition")
      (put! camera/event-queue [:home]))

    "s"
    (when (not= @interaction "edition")
      (.debug js/console "@state-gui/simulation? " (not @state-gui/simulation?))
      (swap! state-gui/simulation? not))

    nil
    ))

(defn window-keydown-evt-on []
  (events/listen js/window EventType.KEYDOWN window-keydown-evt)
  )

(defn window-keydown-evt-off []
  (events/unlisten js/window EventType.KEYDOWN window-keydown-evt)
  )
