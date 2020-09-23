(ns camera.event.zoom
  (:require
   [bubble.coordinate :as coord]
   [camera.core :as core]
   [camera.state :as state]
   [cljs.core.async :refer [put!]]
   [goog.events :as events]
   )
  (:import
   [goog.events EventType]
   ))

(defn- correct-camera-by-translation-fix-point-svg-px
  [camera-origin camera-modified pt-svg-px]
  (let [pt-origin-svg-user   (core/svg-px->svg-user camera-origin pt-svg-px)
        pt-modified-svg-user (core/svg-px->svg-user camera-modified pt-svg-px)
        vec-trans-svg-user (map - pt-modified-svg-user pt-origin-svg-user)
        {:keys [cx cy]} camera-modified
        [new-cx new-cy] (map - [cx cy] vec-trans-svg-user)
        camera-corrected (merge camera-modified {:cx new-cx :cy new-cy})]
    camera-corrected))

(defn- apply-zoom
  "Invariant: the svg-user coordinates associated with the input svg-px must match
  after the camera update, in this case after a zoom factor update."
  [camera scale-factor pt-svg-px]
  (let [camera-zoomed (update camera :zoom * scale-factor)]
    (correct-camera-by-translation-fix-point-svg-px camera camera-zoomed pt-svg-px)))

(defn- apply-resize
  "Invariant: the svg-user coordinates associated with the input svg-px must match
  after the camera update, in this case after a width/height scalar update."
  [camera win-width win-height]
  (let [camera-origin camera
        camera-wider (merge camera-origin {:width win-width :height win-height})
        ;; Take an arbitrary point in the plan as a fix point
        pt-svg-px [0 0]]
    (correct-camera-by-translation-fix-point-svg-px camera camera-wider pt-svg-px)))

(defn mouse-wheel [wheel-delta-y win-px-x win-px-y]
  (let [reduction-speed-factor 5
        scale (.pow js/Math 1.005 (/ wheel-delta-y reduction-speed-factor))
        svg-px (coord/win-px->svg-px [win-px-x win-px-y])
        new-camera (apply-zoom @state/camera scale svg-px)]
    (core/set-camera! new-camera)))

(defn- mouse-wheel-evt [^js/goog.events.Event evt]
  (put! state/event-queue
        [:mouse-wheel (->  evt .getBrowserEvent .-wheelDeltaY) (.-clientX evt) (.-clientY evt)]))

(defn mouse-wheel-evt-on []
  (events/listen js/window EventType.WHEEL mouse-wheel-evt))

(defn mouse-wheel-evt-off []
  (events/unlisten js/window EventType.WHEEL mouse-wheel-evt))

(defn window-resize [width height]
  (let [new-camera
        (apply-resize @state/camera width height)]
    (core/set-camera! new-camera)))

(defn- window-resize-evt []
  (put! state/event-queue
        [:resize (.-innerWidth js/window) (.-innerHeight js/window)]))

(defn window-resize-evt-on []
  (events/listen js/window EventType.RESIZE window-resize-evt))

(defn window-resize-evt-off []
  (events/unlisten js/window EventType.RESIZE window-resize-evt))
