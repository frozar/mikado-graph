(ns bubble.drag
  (:require
   [bubble.drag-utils :as drag-utils]
   ))

(defn dragging-fn
  [event-queue bubble-id]
  (let [if-left-click
        (fn [evt]
          (= 0 (.-button evt)))]
    (fn [evt]
      (when (if-left-click evt)
        (drag-utils/dragging event-queue bubble-id)))))
