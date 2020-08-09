(ns bubble.drag
  (:require
   ;; [bubble.camera :as camera]
   ;; [bubble.coordinate :as coord]
   ;; [bubble.event :as event]
   [bubble.drag-utils :as drag-utils]
   ;; [bubble.state-read :as state-read]
   ;; [cljs.core.async :refer [put!]]
   ;; [goog.events :as events]
   )
  ;; (:import
  ;;  [goog.events EventType])
  )

;; (defn drag-move-fn [event-queue bubble-id]
;;   (let [{init-bubble-cx :cx init-bubble-cy :cy} (state-read/get-bubble bubble-id)
;;         init-mouse-x (atom nil)
;;         init-mouse-y (atom nil)]
;;     (fn [evt]
;;       (let [[mouse-x mouse-y]
;;             (coord/win-px->svg-px [(.-clientX evt) (.-clientY evt)])]
;;         (when (or (nil? @init-mouse-x)
;;                   (nil? @init-mouse-y))
;;           (reset! init-mouse-x mouse-x)
;;           (reset! init-mouse-y mouse-y)
;;           )
;;         (let [scaled-vec-trans-x (camera/scale-dist (- mouse-x @init-mouse-x))
;;               scaled-vec-trans-y (camera/scale-dist (- mouse-y @init-mouse-y))]
;;           (put! event-queue
;;                 [:dragging
;;                  bubble-id
;;                  (+ init-bubble-cx scaled-vec-trans-x)
;;                  (+ init-bubble-cy scaled-vec-trans-y)]))))))

;; (defn drag-end-fn [event-queue bubble-id drag-move drag-end-atom on-end]
;;   (fn []
;;     (put! event-queue [:dragging-end bubble-id])
;;     (events/unlisten js/window EventType.MOUSEMOVE drag-move)
;;     (events/unlisten js/window EventType.MOUSEUP @drag-end-atom)
;;     (on-end)))

;; (defn dragging
;;   ([event-queue bubble-id] (dragging event-queue bubble-id (fn []) (fn [])))
;;   ([event-queue bubble-id on-start on-end]
;;    (let [drag-move (drag-move-fn event-queue bubble-id)
;;          drag-end-atom (atom nil)
;;          drag-end (drag-end-fn event-queue bubble-id drag-move drag-end-atom on-end)]
;;      (on-start)
;;      (put! event-queue [:dragging-start bubble-id])
;;      (reset! drag-end-atom drag-end)
;;      (events/listen js/window EventType.MOUSEMOVE drag-move)
;;      (events/listen js/window EventType.MOUSEUP drag-end))))

(defn dragging-fn
  [event-queue bubble-id]
  (let [if-left-click
        (fn [evt]
          (= 0 (.-button evt)))]
    (fn [evt]
      (when (if-left-click evt)
        (drag-utils/dragging event-queue bubble-id)))))
