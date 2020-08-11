(ns simulation.core-test
  (:require
   [simulation.core :as c]
   [cljs.test :refer (deftest is)]
   ))

(deftest graph-converged?_true
  (let [nodes
        (->
         [{:id "root"     :x 0   :y 0   :group nil :index nil :vy 0 :vx 0}
          {:id "448e2025" :x 100 :y 100 :group nil :index nil :vy 0 :vx 0}]
         clj->js)]
    (is
     (#'c/graph-converged? 1 nodes))))

(deftest graph-converged?_false
  (let [nodes
        (->
         [{:id "root"     :x 0   :y 0   :group nil :index nil :vy 0 :vx 0}
          {:id "448e2025" :x 100 :y 100 :group nil :index nil :vy 1 :vx 0}]
         clj->js)]
    (is
     (not
      (#'c/graph-converged? 1 nodes)))))
