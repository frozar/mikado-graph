(ns bubble.drag-utils
  (:require
   [bubble.camera :as camera]
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.coordinate :as coord]
   [bubble.event-state]
   [bubble.state-read :as state-read]
   [cljs.core.async :refer [put!]]
   [goog.events :as events]
   [simulation.core]
   )
  (:import
   [goog.events EventType]))

(defn- refresh [event-queue simulation?-atom bubble-id]
  (let [connected-graph (state-read/connected-graph (state-read/get-state) ROOT-BUBBLE-ID)
        nb-nodes (-> connected-graph state-read/get-bubbles count)]
    ;; (js/console.log "drag-move-fn (< 1 nb-nodes) " (< 1 nb-nodes))
    (if (and @simulation?-atom
             (state-read/is-connected? (state-read/get-state) ROOT-BUBBLE-ID bubble-id)
             (< 1 nb-nodes))
      (do
        (js/console.log "BEFORE update soft")
        (simulation.core/update-app-state-bubble-position-soft event-queue)
        (state-read/get-bubble bubble-id))
      (state-read/get-bubble bubble-id)))
  )

(defn drag-move-fn [event-queue simulation?-atom bubble-id]
  (let [{init-bubble-cx :cx init-bubble-cy :cy}
        ;; (state-read/get-bubble bubble-id)
        (refresh event-queue simulation?-atom bubble-id)
        init-mouse-x (atom nil)
        init-mouse-y (atom nil)]
    (fn [evt]
      ;; (js/console.log "IN drag-move")
      ;; (js/console.log "simulation? " @bubble.event-state/simulation?)
      ;; (js/console.log "simulation? " @simulation?-atom)

      (let [[mouse-x mouse-y]
            (coord/win-px->svg-px [(.-clientX evt) (.-clientY evt)])]
        (when (or (nil? @init-mouse-x)
                  (nil? @init-mouse-y))
          (reset! init-mouse-x mouse-x)
          (reset! init-mouse-y mouse-y)
          )
        (let [scaled-vec-trans-x (camera/scale-dist (- mouse-x @init-mouse-x))
              scaled-vec-trans-y (camera/scale-dist (- mouse-y @init-mouse-y))]
          (put! event-queue
                [:dragging
                 bubble-id
                 (+ init-bubble-cx scaled-vec-trans-x)
                 (+ init-bubble-cy scaled-vec-trans-y)]))))))

(defn drag-end-fn [event-queue bubble-id drag-move drag-end-atom on-end]
  (fn []
    (put! event-queue [:dragging-end bubble-id])
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (events/unlisten js/window EventType.MOUSEUP @drag-end-atom)
    (on-end)))

(defn dragging
  ([event-queue bubble-id] (dragging event-queue bubble-id (fn []) (fn [])))
  ([event-queue bubble-id on-start on-end]
   ;; (js/console.log "DRAGGING bubble.event-state/simulation? " @bubble.event-state/simulation?)
   (let [drag-move (drag-move-fn event-queue  bubble.event-state/simulation? bubble-id)
         drag-end-atom (atom nil)
         drag-end (drag-end-fn event-queue bubble-id drag-move drag-end-atom on-end)]
     ;; (js/console.log "IN dragging")
     (on-start)
     (put! event-queue [:dragging-start bubble-id])
     (reset! drag-end-atom drag-end)
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))
