(ns bubble.state
  (:require
   [bubble.bubble :as bubble]
   [com.rpl.specter :as sp]
   [reagent.core :as reagent]
   )
  (:require-macros
   [bubble.macro :as macro])
  )

(defn- init-appstate []
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
;;TODO: UT
(defn get-bubble
  ([id] (get-bubble @appstate id))
  ([appstate id]
   (first (filter #(= (:id %) id) (:bubbles appstate)))))

;;TODO: UT
(defn get-bubbles
  ([] (get-bubbles @appstate))
  ([appstate]
   (:bubbles appstate)))

;;TODO: UT
(defn get-list-id
  ([] (get-list-id @appstate))
  ([appstate]
   (->> (get-bubbles appstate)
        (sp/transform [sp/ALL] :id)
        )))

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
       (when (= (:id bubble) id) idx))
     bubbles)))

;; For test purpose
(defn bubble-id-exist [appstate id]
  (let [idx (get-list-id appstate)]
    (not= (some #{id} idx) nil)))

(defn- update-bubble [appstate bubble-id hashmap]
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

(macro/BANG update-bubble)
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

(macro/BANG add-link)

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

(macro/BANG delete-link)

(defn get-links
  ([]
   (get-links @appstate))
  ([appstate]
   (:links appstate)))

;; For test purpose
(defn link-exist [appstate src-id dst-id]
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

(macro/BANG delete-bubble-and-update-link)
;; END: link part

;;TODO: UT
(defn- resize-bubble [appstate bubble-id rx ry]
  (update-bubble appstate bubble-id {:rx rx :ry ry}))

(macro/BANG resize-bubble)

;;TODO: UT
(defn- move-bubble [appstate bubble-id cx cy]
  (update-bubble appstate bubble-id {:cx cx :cy cy}))

(macro/BANG move-bubble)

;;TODO: UT
(defn- save-text-bubble [appstate bubble-id text]
  (update-bubble appstate bubble-id {:text text :initial-state? false}))

(macro/BANG save-text-bubble)

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

(macro/BANG create-bubble-and-link)


;; START: Building link
;;TODO: UT
(defn- set-link-src [appstate id]
  (update appstate :link-src (fn [] id)))

(macro/BANG set-link-src)

(defn get-link-src []
  (:link-src @appstate))

;;TODO: UT
(defn- reset-link-src [appstate]
  (update appstate :link-src (fn [] nil)))

(macro/BANG reset-link-src)

;;TODO: UT
(defn set-mouse-position [appstate mouse-x mouse-y]
  (update appstate :mouse-position (fn [] [mouse-x mouse-y])))

(macro/BANG set-mouse-position)

(defn get-mouse-position []
  (:mouse-position @appstate))

;;TODO: UT
(defn- reset-mouse-position [appstate]
  (update appstate :mouse-position (fn [] nil)))

(macro/BANG reset-mouse-position)

;;TODO: UT
(defn- reset-build-link [appstate]
  (-> appstate
      (reset-link-src)
      (reset-mouse-position)))

(macro/BANG reset-build-link)

;;TODO: UT
(defn- building-link-end [appstate id-dst]
  (let [id-src (get-link-src)]
    (if id-src
      (add-link appstate id-src id-dst)
      appstate)))

(macro/BANG building-link-end)
;; END: Building link


;; START: Edition
(defn- enable-edition [appstate bubble-id]
  (update-bubble appstate bubble-id {:edition? true}))

(defn- disable-edition [appstate bubble-id]
  (update-bubble appstate bubble-id {:edition? false}))

(macro/BANG enable-edition)
(macro/BANG disable-edition)
;; END: Edition


;; START: Show button
(defn- enable-show-button [appstate bubble-id]
  (update-bubble appstate bubble-id {:show-button? true}))

(defn- disable-show-button [appstate bubble-id]
  (update-bubble appstate bubble-id {:show-button? false}))

(macro/BANG enable-show-button)
(macro/BANG disable-show-button)
;; END: Show button


;; START: Toggle done
(defn- toggle-done-status [appstate bubble-id]
  (let [bubble (get-bubble appstate bubble-id)
        new-status (-> (:done? bubble) not)]
    (update-bubble appstate bubble-id {:done? new-status})))

(macro/BANG toggle-done-status)
;; END: Toggle done
