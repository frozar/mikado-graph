(ns bubble.state-read
  (:require
   [bubble.state-data :refer [appstate]]
   ))

;; Read application state

(defn get-bubble
  ([id] (get-bubble @appstate id))
  ([appstate id]
   (first (filter #(= (:id %) id) (:bubbles appstate)))))

(defn get-bubbles
  ([] (get-bubbles @appstate))
  ([appstate]
   (:bubbles appstate)))

(defn get-link-src []
  (:link-src @appstate))

(defn get-links
  ([]
   (get-links @appstate))
  ([appstate]
   (:links appstate)))

(defn get-mouse-position []
  (:mouse-position @appstate))
