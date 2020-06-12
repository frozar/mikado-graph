(ns bubble.state-write
  (:require
   [bubble.bubble :as bubble]
   [bubble.constant :as const]
   [bubble.state :refer [appstate]] ;; used in BANG macro
   [bubble.state-read :as state-read]
   [cljsjs.d3]
   )
  (:require-macros
   [bubble.macro :as macro])
  )

;; Read/Write application state

;; START: bubble part
(defn- add-bubble [appstate bubble-id bubble]
  (update appstate :bubbles conj {bubble-id bubble}))

(defn- delete-bubble [appstate bubble-id]
  (update appstate :bubbles dissoc bubble-id))

(defn- update-bubble [appstate bubble-id hashmap]
  (update-in appstate [:bubbles bubble-id] merge hashmap))

(macro/BANG update-bubble)
;; END: bubble part

;; START: link part
(defn add-link
  ([appstate id-src id-dst]
   (add-link appstate {:src id-src :dst id-dst}))
  ([appstate {id-src :src id-dst :dst
              :as link}]
   (if (and
        (not= id-src id-dst)
        (state-read/bubble-id-exist appstate id-src)
        (state-read/bubble-id-exist appstate id-dst)
        (not (state-read/link-exist appstate id-dst id-src)))
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
     )))

(defn higher-delete-link [appstate filter-func]
  (update appstate :links #(filterv filter-func %)))

(defn- delete-link-involving-bubble [appstate bubble-id]
  (higher-delete-link
   appstate
   (fn [link] (= (some #{bubble-id} (vals link)) nil))))

(defn- delete-link [appstate src-id dst-id]
  (higher-delete-link
   appstate
   (fn [link] (not= {:src src-id :dst dst-id} link))))

(macro/BANG delete-link)

;; For test purpose
(defn- delete-link-to-id-and-update-children-of-id [appstate bubble-id]
  (let [ids-dst (->> (state-read/get-links appstate)
                     (filterv (fn [link] (= bubble-id (:src link))))
                     (map :dst))
        ids-src (->> (state-read/get-links appstate)
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

(defn- gen-id
  "
  Generate a string of length 8, e.g.: 'b56d74c5'
  The generated id is not already present in the
  current application state
  "
  ([]
   (gen-id (state-read/get-list-id)))
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
         (if (some #{id} (state-read/get-list-id appstate))
           (gen-id)
           id)
         new-bubble (bubble/create-bubble not-duplicated-id cx cy)]
     (-> appstate
         (add-bubble not-duplicated-id new-bubble)
         (add-link parent-bubble-id not-duplicated-id))
     ))
  )



(macro/BANG create-bubble-and-link)


;; START: Building link
;;TODO: UT
(defn- set-link-src [appstate id]
  (update appstate :link-src (fn [] id)))

(macro/BANG set-link-src)

;;TODO: UT
(defn- reset-link-src [appstate]
  (update appstate :link-src (fn [] nil)))

(macro/BANG reset-link-src)

;;TODO: UT
(defn set-mouse-position [appstate mouse-x mouse-y]
  (update appstate :mouse-position (fn [] [mouse-x mouse-y])))

(macro/BANG set-mouse-position)

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
  (let [id-src (state-read/get-link-src)]
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


;; START: Toggle done
(defn- toggle-done-status [appstate bubble-id]
  (let [bubble (state-read/get-bubble appstate bubble-id)
        new-status (-> (:done? bubble) not)]
    (update-bubble appstate bubble-id {:done? new-status})))

(macro/BANG toggle-done-status)
;; END: Toggle done

;; START: Toggle rough aspect
;;TODO: UT
(defn- toggle-rough-layout [appstate]
  (let [new-style
        (if (= (appstate :rendering-style) const/REDERING-STYLE-SOLID)
          const/REDERING-STYLE-ROUGH
          const/REDERING-STYLE-SOLID)]
    (update appstate :rendering-style (fn [] new-style))))

(macro/BANG toggle-rough-layout)
;; END: Toggle rough aspect
