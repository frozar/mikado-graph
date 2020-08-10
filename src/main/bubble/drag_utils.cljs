(ns bubble.drag-utils
  (:require
   [bubble.camera :as camera]
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.coordinate :as coord]
   [bubble.event-state]
   [bubble.simulation-to-bubble]
   [bubble.state-read :as state-read]
   [cljs.core.async :refer [put!]]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]))

(defn- update-bubble-position [simulation?-atom bubble-id]
  (let [connected-graph (state-read/connected-graph (state-read/get-state) ROOT-BUBBLE-ID)
        nb-nodes (-> connected-graph state-read/get-bubbles count)]
    ;; (js/console.log "drag-move-fn (< 1 nb-nodes) " (< 1 nb-nodes))
    (if (and @simulation?-atom
             (state-read/is-connected? (state-read/get-state) ROOT-BUBBLE-ID bubble-id)
             (< 1 nb-nodes))
      (do
        ;; (js/console.log "BEFORE update app state bubble position")
        ;; TODO: avoid the modification of the global state here
        (bubble.simulation-to-bubble/update-app-state-bubble-position)
        (state-read/get-bubble bubble-id))
      (state-read/get-bubble bubble-id)))
  )

(defn- delete-links-dom-content []
  (let [links-dom-element
        (js/document.getElementsByClassName "link")
        links-dom-child-element
        (map
         (fn [idx]
           (-> links-dom-element
               (.item idx)
               (.-childNodes)))
         (range (.-length links-dom-element)))]
    ;; (js/console.log "IN when")
    ;; (js/console.log "links-dom-child-element " links-dom-child-element)
    (doseq [child-nodes links-dom-child-element]
      (-> child-nodes
          (.forEach (fn [child-node]
                      ;; (js/console.log "child-node " child-node)
                      (.remove child-node))
                    )))
    ))

(defn drag-move-fn [event-queue simulation?-atom run-at-least-once?-atom bubble-id]
  (let [{init-bubble-cx :cx init-bubble-cy :cy}
        (update-bubble-position simulation?-atom bubble-id)
        init-mouse-x-svg-px (atom nil)
        init-mouse-y-svg-px (atom nil)]
    (fn [evt]
      ;; (js/console.log "IN drag-move ")
      ;; (js/console.log "simulation? " @bubble.event-state/simulation?)
      ;; (js/console.log "simulation? " @simulation?-atom)
      (when-not @run-at-least-once?-atom
        (reset! run-at-least-once?-atom true)
        (delete-links-dom-content))

      (let [[mouse-x-svg-px mouse-y-svg-px]
            (coord/win-px->svg-px [(.-clientX evt) (.-clientY evt)])]
        (when (or (nil? @init-mouse-x-svg-px)
                  (nil? @init-mouse-y-svg-px))
          (reset! init-mouse-x-svg-px mouse-x-svg-px)
          (reset! init-mouse-y-svg-px mouse-y-svg-px)
          )
        (let [scaled-vec-trans-x (camera/scale-dist (- mouse-x-svg-px @init-mouse-x-svg-px))
              scaled-vec-trans-y (camera/scale-dist (- mouse-y-svg-px @init-mouse-y-svg-px))]
          (put! event-queue
                [:dragging
                 bubble-id
                 (+ init-bubble-cx scaled-vec-trans-x)
                 (+ init-bubble-cy scaled-vec-trans-y)]))))))

(defn drag-end-fn [event-queue run-at-least-once?-atom bubble-id drag-move drag-end-atom on-end]
  (fn []
    (when @run-at-least-once?-atom
      (put! event-queue [:dragging-end bubble-id]))
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (events/unlisten js/window EventType.MOUSEUP @drag-end-atom)
    (on-end)))

(defn dragging
  ([event-queue bubble-id] (dragging event-queue bubble-id (fn []) (fn [])))
  ([event-queue bubble-id on-start on-end]
   ;; (js/console.log "DRAGGING bubble.event-state/simulation? " @bubble.event-state/simulation?)
   (let [run-at-least-once? (atom false)
         drag-move (drag-move-fn event-queue bubble.event-state/simulation? run-at-least-once? bubble-id)
         drag-end-atom (atom nil)
         drag-end (drag-end-fn event-queue run-at-least-once? bubble-id drag-move drag-end-atom on-end)]
     ;; (js/console.log "IN dragging")
     (on-start)
     (reset! drag-end-atom drag-end)
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))
