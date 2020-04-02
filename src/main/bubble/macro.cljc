(ns bubble.macro
  (:require
   [cljs.analyzer :as cljs]))

(defmacro BANG
  "Take a func-name as input and generate the banged function associated.
  Should be used in bubble.state* namespaces only.

  For example:
  (bubble.macro/BANG update-bubble)
  => (defn update-bubble! [bubble-id hashmap]
       (swap! appstate #(update-bubble % bubble-id hashmap)))
  "
  [func-name]
  (let [
        func-name-banged
        (symbol (str func-name "!"))

        arglists-but-first
        (-> (cljs/resolve-var &env func-name) :meta :arglists second first rest)
        ]
    `(defn ~func-name-banged [~@arglists-but-first]
       (swap! bubble.state-data/appstate #(~func-name % ~@arglists-but-first)))))
