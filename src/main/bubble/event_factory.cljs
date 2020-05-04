(ns bubble.event-factory
  (:require
   [bubble.build-link :as build-link]
   [bubble.constant :as const]
   [bubble.drag :as drag]
   [bubble.state-read :as state-read]
   [bubble.event :as event]
   [cljs.core.async :refer [put!]]
   ))

(defn event-property-factory
  ;; Take a link as input
  [shape & args]

  (case shape
    :link
    (let [[src-id dst-id] args]
      {:on-context-menu
       #(put! event/event-queue [:delete-link src-id dst-id])})

    :pencil-button
    (let [[bubble] args
          {:keys [id]} bubble]
      {:on-click
       #(put! event/event-queue [:enable-edition id])})

    :link-button
    (let [[bubble] args
          {:keys [id]} bubble]
      {:pointer-events "bounding-box"
       :on-click
       (build-link/build-link-start-fn id)})

    :delete-button
    (let [[bubble] args
          {:keys [id]} bubble]
      {:on-click
       #(put! event/event-queue [:delete-bubble id])})

    :validation-button
    (let [[bubble] args
          {:keys [id]} bubble]
      {:pointer-events "bounding-box"
       :on-click
       #(put! event/event-queue [:toggle-done-status id])})

    :bubble
    (let [[bubble] args
          {:keys [id]} bubble]
      {:pointer-events "bounding-box"
       :on-mouse-over
       (if (state-read/get-link-src)
         #(put! event/event-queue [:disable-show-button id])
         #(put! event/event-queue [:enable-show-button id])
         )
       :on-mouse-leave
       #(put! event/event-queue [:disable-show-button id])})

    :common-text-ellipse
    (let [[bubble] args
          {:keys [id type]} bubble]
      {:on-mouse-down
       (fn [evt]
         ;; It must be a simple click
         (when (not (.-ctrlKey evt))
           ((drag/dragging-fn id) evt)))

       :on-context-menu
       (when (not= type const/ROOT-BUBBLE-TYPE)
         (event/prevent-default
          #(put! event/event-queue [:delete-bubble id])))

       :on-click
       (fn
         ;; "If the 'ctrl' is press during a click, build a link.
         ;; Else, build a link with the current bubble.
         ;; "
         [evt]
         (if (.-ctrlKey evt)
           ((build-link/build-link-start-fn id) evt)
           (build-link/build-link-end id)
           )
         )
       })

    :text
    (let [[bubble] args
          {:keys [id]} bubble]
      (merge
       (event-property-factory :common-text-ellipse bubble)
       {:on-double-click
        #(put! event/event-queue [:enable-edition id])
        }))

    :ellipse
    (let [[bubble ry] args
          {:keys [id cx cy]} bubble]
      (merge
       (event-property-factory :common-text-ellipse bubble)
       {:on-double-click
        (let [[new-cx new-cy] [cx (- cy (* 3 ry ))]]
          #(put! event/event-queue [:create-bubble id new-cx new-cy]))
        }))))
