(ns bubble.state
  (:require [bubble.constant :as const]
            [reagent.core :as reagent]
            [com.rpl.specter :as sp]
            )
  )

(def nil-bubble
  {:id const/NIL-BUBBLE-ID
   :type const/NIL-BUBBLE-TYPE
   :initial-state? true
   :done? false
   :edition? false
   :cx 0
   :cy 0
   :rx 100 :ry 50
   :text const/BUBBLE-DEFAULT-TEXT
   })

(def root-bubble
  (merge nil-bubble
         {:id const/ROOT-BUBBLE-ID
          :type const/ROOT-BUBBLE-TYPE
          :cx 450
          :cy 450
          :text const/ROOT-BUBBLE-DEFAULT-TEXT
          }))

(defn init-appstate []
  {
   :bubbles [root-bubble]
   :links []
   :link-src nil
   :mouse-position nil
   })

(defonce appstate
  (reagent/atom (init-appstate)))

;; Read/Write application state

(defn get-bubble
  ([id] (get-bubble @appstate id))
  ([appstate id]
   (first (filter #(= (:id %) id) (:bubbles appstate)))))

(defn get-all-bubble []
  (:bubbles @appstate))

(defn get-bubble-but-root []
  (filter #(not= (:id %) const/ROOT-BUBBLE-ID) (:bubbles @appstate)))

(defn add-bubble [appstate bubble]
  (update appstate :bubbles conj bubble))

(defn add-bubble! [bubble]
  (swap! appstate #(add-bubble % bubble)))

(defn delete-bubble-shape [bubble-id]
  (swap! appstate update :bubbles (fn [l] (filterv #(not (= (:id %) bubble-id)) l))))

(defn add-link [id-src id-dst]
  (swap! appstate update :links conj {:src id-src :dst id-dst}))

(defn delete-link-to-bubble [bubble-id]
  (swap! appstate update :links (fn [l] (filterv
                                             (fn [link] not (= (some #{bubble-id} (vals link)) nil)) l))))

(defn delete-link [src-id dst-id]
  (swap! appstate update :links (fn [links]
                                      (filterv
                                       (fn [link] (not= {:src src-id :dst dst-id} link))
                                       links))))

(defn update-link [bubble-id]
  (let [ids-dst (->> (:links @appstate)
                     (filterv (fn [link] (= bubble-id (:src link))))
                     (map :dst))
        ids-src (->> (:links @appstate)
                     (filterv (fn [link] (= bubble-id (:dst link))))
                     (map :src))
        new-links (vec (for [id-src ids-src
                             id-dst ids-dst]
                         {:src id-src :dst id-dst}))
        ]
    (delete-link-to-bubble bubble-id)
    (swap! appstate update :links (fn [l] (->> (apply conj l new-links)
                                             set
                                             vec)))
    ))

(defn get-links []
  (:links @appstate))

(defn resize-bubble [bubble-id rx ry]
  (swap! appstate update :bubbles
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
  (fn [cx cy]
    (swap!
     appstate update :bubbles
     (fn [list-bubble]
       (let [list-idxs (map #(:id %) list-bubble)]
         ;; Check if the id to delete is present in the model.
         ;; When the user drag a bubble on a right click, the move action
         ;; associated try to delete an id which was ready deleted.
         (if (some #{id} list-idxs)
           (update
            list-bubble
            (.indexOf list-idxs id)
            (fn [b] (merge b {:cx cx :cy cy})))
           ;; Else body
           list-bubble))))))

(defn save-text-bubble [id text default-text]
  (swap!
   appstate update :bubbles
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

(defn gen-id
  "Generate a string of length 8, e.g.: 'b56d74c5'
  This generated id is not already present in the current application state"
  ([]
   (let [list-id (->> (get-all-bubble)
                      (map :id))]
     (gen-id list-id)))
  ([list-id]
   (let [try-id (apply str (repeatedly 8 #(rand-nth "0123456789abcdef")))]
     (if (some #{try-id} list-id)
       (recur list-id)
       try-id))))

(defn- get-new-bubble
  ([cx cy] (get-new-bubble cx cy (gen-id)))
  ([cx cy id]
   (merge
    nil-bubble
    {:id id
     :type const/BUBBLE-TYPE
     :cx cx
     :cy cy})))

(defn create-bubble-and-link [parent-bubble-id cx cy]
  (let [bubble-id (gen-id)
        new-bubble (get-new-bubble cx cy bubble-id)]
    (add-bubble! new-bubble)
    (add-link parent-bubble-id bubble-id)
    )
  )

(defn delete-bubble [bubble-id]
  (delete-bubble-shape bubble-id)
  (update-link bubble-id))

;; START: Building link
(defn set-link-src [id]
  (swap! appstate update :link-src (fn [] id)))

(defn get-link-src []
  (:link-src @appstate))

(defn reset-link-src []
  (swap! appstate update :link-src (fn [] nil)))

(defn set-mouse-position [mouse-x mouse-y]
  (swap! appstate update :mouse-position (fn [] [mouse-x mouse-y])))

(defn get-mouse-position []
  (:mouse-position @appstate))

(defn reset-mouse-position []
  (swap! appstate update :mouse-position (fn [] nil)))

(defn reset-build-link []
  (do
    (reset-link-src)
    (reset-mouse-position))
  )

(defn building-link-end [id-dst]
  (let [id-src (get-link-src)]
    (add-link id-src id-dst)
    ))
;; END: Building link

;; START: Edition
(defn- enable-edition [appstate id]
  (let [get-idx-by-bubble-id
        (fn [id]
          (fn [bubbles]
            (keep-indexed
             (fn [idx val]
               (if (= (:id val) id) idx))
             bubbles)))]
    (sp/transform
     [:bubbles
      (sp/srange-dynamic
       (fn [bubbles]
         (-> bubbles
             ((fn [bubbles] ((get-idx-by-bubble-id id) bubbles)))
             first)
         )
       (fn [bubbles]
         (-> bubbles
              ((fn [bubbles] ((get-idx-by-bubble-id id) bubbles)))
              first
              inc)
         )
       )
      sp/ALL
      ]
     (fn [bubble]
       (merge bubble {:edition? true}))
     appstate))
  )

(defn enable-edition! [id]
  (swap! appstate #(enable-edition % id)))
;; END: Edition
