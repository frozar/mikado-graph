(ns bubble.state
  (:require [bubble.geometry :as g]
            [bubble.constant :as const]
            [reagent.core :as reagent]
            )
  )

(def nil-bubble
  {:id const/NIL-BUBBLE-ID
   :type const/NIL-BUBBLE-TYPE
   :initial-state? true
   :done? false
   :center (g/point 0 0)
   :rx 100 :ry 50
   :text const/BUBBLE-DEFAULT-TEXT
   })

(def root-bubble
  (merge nil-bubble
         {:id const/ROOT-BUBBLE-ID
          :type const/ROOT-BUBBLE-TYPE
          :initial-state? true
          :done? false
          :center (g/point 450 450)
          :rx 100
          :ry 50
          :text const/ROOT-BUBBLE-DEFAULT-TEXT
          }))

(defn initial-application-state []
  {
   :bubbles [root-bubble]
   :links []
   :link-src nil
   })

(defonce points
  (reagent/atom (initial-application-state)))

;; Read/Write application state
(defn set-link-src [id]
  (swap! points update :link-src (fn [] id))
  )

(defn get-link-src []
  (:link-src @points)
  )

(defn reset-link-src []
  (swap! points update :link-src (fn [] nil)))

(defn get-bubble [id]
  (first (filter #(= (:id %) id) (:bubbles @points))))

(defn get-all-bubble []
  (:bubbles @points))

(defn get-bubble-but-root []
  (filter #(not= (:id %) const/ROOT-BUBBLE-ID) (:bubbles @points)))

(defn add-bubble [bubble]
  (swap! points update :bubbles conj bubble))

(defn delete-bubble-shape [bubble-id]
  (swap! points update :bubbles (fn [l] (filterv #(not (= (:id %) bubble-id)) l))))

(defn add-link [id-src id-dst]
  (swap! points update :links conj {:src id-src :dst id-dst}))

(defn delete-link-to-bubble [bubble-id]
  (swap! points update :links (fn [l] (filterv
                                             (fn [link] not (= (some #{bubble-id} (vals link)) nil)) l))))

(defn delete-link [src-id dst-id]
  (swap! points update :links (fn [links]
                                      (filterv
                                       (fn [link] (not= {:src src-id :dst dst-id} link))
                                       links))))

(defn update-link [bubble-id]
  (let [ids-dst (->> (:links @points)
                     (filterv (fn [link] (= bubble-id (:src link))))
                     (map :dst))
        ids-src (->> (:links @points)
                     (filterv (fn [link] (= bubble-id (:dst link))))
                     (map :src))
        new-links (vec (for [id-src ids-src
                             id-dst ids-dst]
                         {:src id-src :dst id-dst}))
        ]
    (delete-link-to-bubble bubble-id)
    (swap! points update :links (fn [l] (->> (apply conj l new-links)
                                             set
                                             vec)))
    ))

(defn resize-bubble [bubble-id rx ry]
  (swap! points update :bubbles
         (fn [list-bubble]
           (let [list-idxs (map #(:id %) list-bubble)]
             ;; Check if the id to resize is present in the model.
             (if (some #{bubble-id} list-idxs)
               (update
                list-bubble
                (.indexOf list-idxs bubble-id)
                (fn [b] (merge b {:rx rx :ry ry}))
                ;; Else body
                list-bubble))))))


(defn move-bubble [id]
  (fn [x y]
    (swap!
     points update :bubbles
     (fn [list-bubble]
       (let [list-idxs (map #(:id %) list-bubble)]
         ;; Check if the id to delete is present in the model.
         ;; When the user drag a bubble on a right click, the move action
         ;; associated try to delete an id which was ready deleted.
         (if (some #{id} list-idxs)
           (update
            list-bubble
            (.indexOf list-idxs id)
            (fn [b] (merge b {:center (g/point x y)})))
           ;; Else body
           list-bubble))))))

(defn save-text-bubble [id text default-text]
  (swap!
   points update :bubbles
   (fn [list-bubble]
     (let [list-idxs (map #(:id %) list-bubble)]
       ;; Check if the id to delete is present in the model.
       ;; When the user drag a bubble on a right click, the move action
       ;; associated try to delete an id which was ready deleted.
       (if (some #{id} list-idxs)
         (do
           (let [current-bubble (get-bubble id)
                 initial-state-value (and (= text default-text)
                                          (:initial-state? current-bubble))]
             (update
              list-bubble
              (.indexOf list-idxs id)
              #(merge % {:text text :initial-state? initial-state-value}))))
         ;; Else body
         list-bubble)))))
