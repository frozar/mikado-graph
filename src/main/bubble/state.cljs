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

;;TODO: Write a macro to generete the bang (!) version
;;      of relevant function

;; Read/Write application state

;; START: bubble part
;;TODO: add a unit test
(defn get-bubble
  ([id] (get-bubble @appstate id))
  ([appstate id]
   (first (filter #(= (:id %) id) (:bubbles appstate)))))

;;TODO: add a unit test
(defn- get-bubbles
  ([] (get-bubbles @appstate))
  ([appstate]
   (:bubbles appstate)))

;;TODO: add a unit test
(defn get-list-id
  ([] (get-list-id @appstate))
  ([appstate]
   (->> (get-bubbles appstate)
        (sp/transform [sp/ALL] :id)
        )))

;;TODO: add a unit test
(defn get-bubble-but-root []
  (filterv #(not= (:id %) const/ROOT-BUBBLE-ID) (:bubbles @appstate)))

(defn- add-bubble [appstate bubble]
  (update appstate :bubbles conj bubble))

(defn- delete-bubble [appstate bubble-id]
  (sp/transform
   [:bubbles]
   (fn [bubbles]
     (filterv (fn [bubble] (not= (:id bubble) bubble-id)) bubbles))
   appstate)
  )

(defn- get-bubble-idx-by-id [id]
  (fn [bubbles]
    (keep-indexed
     (fn [idx bubble]
       (if (= (:id bubble) id) idx))
     bubbles)))

(defn- bubble-id-exist [appstate id]
  (let [idx (get-list-id appstate)]
    (not= (some #{id} idx) nil)))

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
(defn add-link
  ([appstate id-src id-dst]
   (add-link appstate {:src id-src :dst id-dst}))
  ([appstate link]
   (if (and
        (bubble-id-exist appstate (:src link))
        (bubble-id-exist appstate (:dst link)))
     (update appstate
             :links
             (fn [links] (-> (conj links link)
                             set
                             vec))
             )
     appstate)))

(defn add-link! [id-src id-dst]
  (swap! appstate #(add-link % id-src id-dst)))

(defn add-links
  ([appstate links]
   (if (empty? links)
     appstate
     (add-links appstate (first links) (rest links))))
  ([appstate link links]
   (let [new-appstate
         (add-link appstate link)]
     (if (empty? links)
       new-appstate
       (recur new-appstate (first links) (rest links)))
     ))
  )

(defn higher-delete-link [appstate filter-func]
  (sp/transform
   [:links]
   #(filterv filter-func %)
   appstate)
  )

(defn- delete-link-involving-bubble [appstate bubble-id]
  (higher-delete-link
   appstate
   (fn [link] (= (some #{bubble-id} (vals link)) nil))))

(defn- delete-link [appstate src-id dst-id]
  (higher-delete-link
   appstate
   (fn [link] (not= {:src src-id :dst dst-id} link))))

(defn delete-link! [src-id dst-id]
  (swap! appstate #(delete-link % src-id dst-id)))

(defn- get-links
  ([]
   (get-links @appstate))
  ([appstate]
   (:links appstate)))

(defn- link-exist [appstate src-id dst-id]
  (let [links (get-links appstate)]
    (not= (some #{{:src src-id :dst dst-id}} links) nil)))

(defn- delete-link-to-id-and-update-children-of-id [appstate bubble-id]
  (let [ids-dst (->> (get-links appstate)
                     (filterv (fn [link] (= bubble-id (:src link))))
                     (map :dst))
        ids-src (->> (get-links appstate)
                     (filterv (fn [link] (= bubble-id (:dst link))))
                     (map :src))
        new-links (vec (for [id-src ids-src
                             id-dst ids-dst]
                         {:src id-src :dst id-dst}))
        ]
    (-> appstate
        (delete-link-involving-bubble bubble-id)
        (add-links new-links)
        )
    ))

(defn- delete-bubble-and-update-link [appstate bubble-id]
  (-> appstate
      (delete-bubble bubble-id)
      (delete-link-to-id-and-update-children-of-id bubble-id)))

(defn delete-bubble-and-update-link! [bubble-id]
  (swap! appstate
         #(delete-bubble-and-update-link % bubble-id)))
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

(defn save-text-bubble! [bubble-id text]
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

(defn- disable-edition [appstate bubble-id]
  (update-bubble appstate bubble-id {:edition? false}))

(defn disable-edition! [bubble-id]
  (swap! appstate #(disable-edition % bubble-id)))
;; END: Edition

;; START: Show button
(defn- enable-show-button [appstate bubble-id]
  (update-bubble appstate bubble-id {:show-button? true}))

(defn enable-show-button! [bubble-id]
  (swap! appstate #(enable-show-button % bubble-id)))

(defn- disable-show-button [appstate bubble-id]
  (update-bubble appstate bubble-id {:show-button? false}))

(defn disable-show-button! [bubble-id]
  (swap! appstate #(disable-show-button % bubble-id)))
;; END: Show button

;; START: Toggle done
(defn- toggle-done-status [appstate bubble-id]
  (let [bubble (get-bubble bubble-id)
        new-status (-> (:done? bubble) not)]
    (update-bubble appstate bubble-id {:done? new-status})))

(defn toggle-done-status! [bubble-id]
  (swap! appstate #(toggle-done-status % bubble-id)))
;; END: Toggle done
