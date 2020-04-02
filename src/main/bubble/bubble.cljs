(ns bubble.bubble
  (:require
   [bubble.constant :as const])
)

(def nil-bubble
  {:id const/NIL-BUBBLE-ID
   :type const/NIL-BUBBLE-TYPE
   :initial-state? true
   :done? false
   :edition? false
   :show-button? false
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

(defn create-bubble
  [id cx cy]
  (merge
   nil-bubble
   {:id id
    :type const/BUBBLE-TYPE
    :cx cx
    :cy cy}))

(defn update-bubble
  "This function update any key of a bubble, except its id"
  [bubble hashmap]
  (merge bubble (dissoc hashmap :id)))
