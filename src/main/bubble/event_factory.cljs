(ns bubble.event-factory
  (:require
   [bubble.build-link :as build-link]
   [bubble.constant :as const]
   [bubble.drag :as drag]
   ;; [bubble.event :as event]
   [bubble.event-util :as event-util]
   [bubble.state-gui :refer [event-queue]]
   [cljs.core.async :refer [put!]]
   ))

(defn event-property-factory
  "NB.: Many elements have to stop the propagation of the :on-mouse-down event.
  This is done to avoid the panning of the parent svg canvas."
  [shape & args]

  (case shape
    :link
    (let [[src-id dst-id] args]
      {:on-context-menu
       #(put! event-queue [:delete-link src-id dst-id])})

    :pencil-button
    (let [[bubble] args
          {:keys [id]} bubble]
      {:pointer-events "bounding-box"

       :on-click
       #(put! event-queue [:enable-edition id])

       :on-mouse-down
       (fn [evt]
         ;; Avoid the propagation of the event to the parent canvas
         (.stopPropagation evt))})

    :link-button
    (let [[bubble] args
          {:keys [id]} bubble]
      {:pointer-events "bounding-box"

       :on-click
       (build-link/build-link-start-fn event-queue id)

       :on-mouse-down
       (fn [evt]
         ;; Avoid the propagation of the event to the parent canvas
         (.stopPropagation evt))})

    :delete-button
    (let [[bubble] args
          {:keys [id]} bubble]
      {:pointer-events "bounding-box"

       :on-click
       #(put! event-queue [:delete-bubble id])

       :on-mouse-down
       (fn [evt]
         ;; Avoid the propagation of the event to the parent canvas
         (.stopPropagation evt))})

    :validation-button
    (let [[bubble] args
          {:keys [id]} bubble]
      {:pointer-events "bounding-box"
       :on-click
       #(put! event-queue [:toggle-done-status id])

       :on-mouse-down
       (fn [evt]
         ;; Avoid the propagation of the event to the parent canvas
         (.stopPropagation evt))})

    :common-text-ellipse
    (let [[bubble] args
          {:keys [id type]} bubble]
      {:on-mouse-down
       (fn [evt]
         ;; It must be a simple click
         (when (not (.-ctrlKey evt))
           ((drag/dragging-fn event-queue id) evt))

         ;; Avoid the propagation of the event to the parent canvas
         (.stopPropagation evt))

       :on-context-menu
       (when (not= type const/ROOT-BUBBLE-TYPE)
         (event-util/prevent-default
          #(put! event-queue [:delete-bubble id])))

       :on-click
       (fn
         ;; "If the 'ctrl' is press during a click, build a link.
         ;; Else, build a link with the current bubble.
         ;; "
         [evt]
         (if (.-ctrlKey evt)
           ((build-link/build-link-start-fn event-queue id) evt)
           (build-link/build-link-end event-queue id)
           )
         )
       })

    :text
    (let [[bubble] args
          {:keys [id]} bubble]
      (merge
       (event-property-factory :common-text-ellipse bubble)
       {:on-double-click
        #(put! event-queue [:enable-edition id])
        }))

    :ellipse
    (let [[bubble ry] args
          {:keys [id cx cy]} bubble]
      (merge
       (event-property-factory :common-text-ellipse bubble)
       {:on-double-click
        (let [[new-cx new-cy] [cx (- cy (* 3 ry ))]]
          #(put! event-queue [:create-bubble id new-cx new-cy]))
        }))))
