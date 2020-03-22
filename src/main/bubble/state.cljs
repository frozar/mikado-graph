(ns bubble.state
  (:require [bubble.bubble :as bubble]
            [bubble.constant :as const]
            [reagent.core :as reagent]
            [com.rpl.specter :as sp]
            )
  )

(defn init-appstate []
  {
   :bubbles [bubble/root-bubble]
   :links []
   :link-src nil
   :mouse-position nil
   })

(defonce appstate
  (reagent/atom (init-appstate)))

;; Read/Write application state

;; START: bubble part
(defn get-bubble
  ([id] (get-bubble @appstate id))
  ([appstate id]
   (first (filter #(= (:id %) id) (:bubbles appstate)))))

(defn get-all-bubble
  ([] (get-all-bubble @appstate))
  ([appstate]
   (:bubbles appstate)))

(defn get-list-id
  ([] (get-list-id @appstate))
  ([appstate]
   (->> (get-all-bubble appstate)
        (sp/transform [sp/ALL] :id)
        )))

(defn get-bubble-but-root []
  (filter #(not= (:id %) const/ROOT-BUBBLE-ID) (:bubbles @appstate)))

(defn- add-bubble [appstate bubble]
  (update appstate :bubbles conj bubble))

(defn- delete-bubble [appstate bubble-id]
  (sp/transform
   [:bubbles]
   (fn [bubbles]
     (filter (fn [bubble] (not= (:id bubble) bubble-id)) bubbles))
   appstate)
  )

;;TODO: Delete this angerous function
(defn delete-bubble! [bubble-id]
  (swap! appstate #(delete-bubble % bubble-id)))

(defn- get-bubble-idx-by-id [id]
  (fn [bubbles]
    (keep-indexed
     (fn [idx bubble]
       (if (= (:id bubble) id) idx))
     bubbles)))

(defn update-bubble [appstate bubble-id hashmap]
  (sp/transform
   [:bubbles
    (sp/srange-dynamic
     (fn [bubbles]
       (-> bubbles
           ((fn [bubbles] ((get-bubble-idx-by-id bubble-id) bubbles)))
           first)
       )
     (fn [bubbles]
       (-> bubbles
           ((fn [bubbles] ((get-bubble-idx-by-id bubble-id) bubbles)))
           first
           inc)
       )
     )
    sp/ALL
    ]
   (fn [bubble]
     (bubble/update-bubble bubble hashmap)
     )
   appstate))

(defn update-bubble! [bubble-id hashmap]
  (swap! appstate #(update-bubble % bubble-id hashmap)))
;; END: bubble part


;; START: link part
(defn add-link [appstate id-src id-dst]
  (update appstate :links conj {:src id-src :dst id-dst}))

(defn add-link! [id-src id-dst]
  (swap! appstate #(add-link % id-src id-dst)))

(defn delete-link-to-bubble [bubble-id]
  (swap! appstate update :links (fn [l] (filterv
                                             (fn [link] not (= (some #{bubble-id} (vals link)) nil)) l))))

(defn delete-link [src-id dst-id]
  (swap! appstate update :links (fn [links]
                                      (filterv
                                       (fn [link] (not= {:src src-id :dst dst-id} link))
                                       links))))

(defn- get-links
  ([]
   (get-links @appstate))
  ([appstate]
   (:links appstate)))

(defn delete-link-to-id-and-update-children-of-id! [bubble-id]
  (let [ids-dst (->> (get-links)
                     (filterv (fn [link] (= bubble-id (:src link))))
                     (map :dst))
        ids-src (->> (get-links)
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

(defn delete-bubble-and-update-link! [bubble-id]
  (delete-bubble! bubble-id)
  (delete-link-to-id-and-update-children-of-id! bubble-id))
;; END: link part

;;TODO: add a unit test
(defn- resize-bubble [appstate bubble-id rx ry]
  (update-bubble appstate bubble-id {:rx rx :ry ry}))

(defn resize-bubble! [bubble-id rx ry]
  (swap! appstate #(resize-bubble % bubble-id rx ry)))

;;TODO: add a unit test
(defn- move-bubble [appstate bubble-id cx cy]
  (update-bubble appstate bubble-id {:cx cx :cy cy}))

(defn move-bubble! [bubble-id cx cy]
  (swap! appstate #(move-bubble % bubble-id cx cy)))

;;TODO: add a unit test
(defn- save-text-bubble [appstate bubble-id text]
  (update-bubble appstate bubble-id {:text text :initial-state? false}))

(defn save-text-bubble! [bubble-id text default-text]
  (swap! appstate #(save-text-bubble % bubble-id text)))

(defn gen-id
  "
  Generate a string of length 8, e.g.: 'b56d74c5'
  The generated id is not already present in the
  current application state
  "
  ([]
   (gen-id (get-list-id)))
  ([list-id]
   (let [try-id (apply str (repeatedly 8 #(rand-nth "0123456789abcdef")))]
     (if (some #{try-id} list-id)
       (recur list-id)
       try-id))))

(defn- create-bubble-and-link
  ([appstate parent-bubble-id cx cy]
   (create-bubble-and-link appstate parent-bubble-id cx cy (gen-id)))
  ([appstate parent-bubble-id cx cy id]
   (let [not-duplicated-id
         (if (some #{id} (get-list-id appstate))
           (gen-id)
           id)
         new-bubble (bubble/create-bubble cx cy not-duplicated-id)]
     (-> appstate
         (add-bubble new-bubble)
         (add-link parent-bubble-id not-duplicated-id))
     ))
  )

(defn create-bubble-and-link! [parent-bubble-id cx cy]
  (swap! appstate #(create-bubble-and-link % parent-bubble-id cx cy))
  )

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

;;TODO: avoid to use add-link! directly
(defn building-link-end [id-dst]
  (let [id-src (get-link-src)]
    (add-link! id-src id-dst)
    ))
;; END: Building link

;; START: Edition
(defn- enable-edition [appstate bubble-id]
  (update-bubble appstate bubble-id {:edition? true}))

(defn enable-edition! [bubble-id]
  (swap! appstate #(enable-edition % bubble-id)))

;; END: Edition
