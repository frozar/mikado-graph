(ns bubble.event-util)

(defn prevent-default
  ([] (prevent-default (fn [])))
  ([func]
   (fn [evt]
     (.preventDefault evt)
     (func))))
