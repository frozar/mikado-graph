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

(defn top-bubble [{:keys [cy ry]}]
  (- cy ry))

(defn bottom-bubble [{:keys [cy ry]}]
  (+ cy ry))

(defn left-bubble [{:keys [cx rx]}]
  (- cx rx))

(defn right-bubble [{:keys [cx rx]}]
  (+ cx rx))

(defn bbox-area-bubble [bubble]
  (let [top (top-bubble bubble)
        bottom (bottom-bubble bubble)
        left (left-bubble bubble)
        right (right-bubble bubble)
        width (- right left)
        height (- bottom top)]
    (* width height)))
