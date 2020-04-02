(ns bubble.state-read
  (:require
   [bubble.state :refer [appstate]]
   [com.rpl.specter :as sp]
   ))

;; Read application state

;; START: bubble part
(defn get-bubble
  ([id] (get-bubble @appstate id))
  ([appstate id]
   (first (filter #(= (:id %) id) (:bubbles appstate)))))

(defn get-bubbles
  ([] (get-bubbles @appstate))
  ([appstate]
   (:bubbles appstate)))

(defn get-list-id
  ([] (get-list-id @appstate))
  ([appstate]
   (->> (get-bubbles appstate)
        (sp/transform [sp/ALL] :id)
        )))

(defn bubble-id-exist [appstate id]
  (let [idx (get-list-id appstate)]
    (not= (some #{id} idx) nil)))
;; END: bubble part

;; START: link part

(defn get-link-src []
  (:link-src @appstate))

(defn get-links
  ([]
   (get-links @appstate))
  ([appstate]
   (:links appstate)))

(defn link-exist [appstate src-id dst-id]
  (let [links (get-links appstate)]
    (not= (some #{{:src src-id :dst dst-id}} links) nil)))
;; END: link part

(defn get-mouse-position []
  (:mouse-position @appstate))
