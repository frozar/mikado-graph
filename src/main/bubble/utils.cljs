(ns bubble.utils
  (:require [bubble.state :as state]))

(defn gen-id
  "Generate a string of length 8, e.g.: 'b56d74c5'
  This generated id is not already present in the current application state"
  ([]
   (let [list-id (->> (state/get-all-bubble)
                      (map :id))]
     (gen-id list-id)))
  ([list-id]
   (let [try-id (apply str (repeatedly 8 #(rand-nth "0123456789abcdef")))]
     (if (some #{try-id} list-id)
       (recur list-id)
       try-id))))
