(ns simulation.core-test
  (:require
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.state :as sd]
   [bubble.state-write :as sw]
   [simulation.core :as c]
   [cljs.test :refer (deftest is)]
   ))

(def appstate-1-bubble
  (#'sd/init-appstate)
  )

(deftest appstate->graph_basic
  (is
   (=
    (#'c/appstate->graph appstate-1-bubble)
    {:nodes [{:id ROOT-BUBBLE-ID, :x 450, :y 450 :group 1}]
     :links []}))
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        (#'c/appstate->graph))
    {:nodes [{:id ROOT-BUBBLE-ID, :x 450, :y 450 :group 1}
             {:id "bubble-1", :x 450, :y 450, :group 1}]
     :links [{:source ROOT-BUBBLE-ID, :target "bubble-1", :value 10}]}))
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-2")
        (#'c/appstate->graph))
    {:nodes [{:id ROOT-BUBBLE-ID, :x 450, :y 450 :group 1}
             {:id "bubble-1", :x 450, :y 450, :group 1}
             {:id "bubble-2", :x 450, :y 450, :group 1}]
     :links [{:source ROOT-BUBBLE-ID, :target "bubble-1", :value 10}
             {:source ROOT-BUBBLE-ID, :target "bubble-2", :value 10}]})))

(deftest graph-distance_identity
  (let [nodes
        [{:id "root"     :x 0   :y 0   :group nil :index nil :vy nil :vx nil}
         {:id "448e2025" :x 100 :y 100 :group nil :index nil :vy nil :vx nil}]
        previous-nodes nodes]
    (is
     (=
      (#'c/graph-distance previous-nodes nodes)
      0))))

(deftest graph-distance_small_diff
  (let [nodes
        [{:id "root"     :x 0   :y 0   :group nil :index nil :vy nil :vx nil}
         {:id "448e2025" :x 100 :y 100 :group nil :index nil :vy nil :vx nil}]
        previous-nodes
        [{:id "root"     :x 0   :y 0   :group nil :index nil :vy nil :vx nil}
         {:id "448e2025" :x 100 :y 101 :group nil :index nil :vy nil :vx nil}]]
    (is
     (=
      (#'c/graph-distance previous-nodes nodes)
      1))))

(deftest graph-distance_large_diff
  (let [nodes
        [{:id "root"     :x 0   :y 0   :group nil :index nil :vy nil :vx nil}
         {:id "448e2025" :x 100 :y 100 :group nil :index nil :vy nil :vx nil}]
        previous-nodes
        [{:id "root"     :x 0   :y 0   :group nil :index nil :vy nil :vx nil}
         {:id "448e2025" :x 100 :y 200 :group nil :index nil :vy nil :vx nil}]]
    (is
     (=
      (#'c/graph-distance previous-nodes nodes)
      10000))))

(deftest graph-distance_more_nodes
  (let [nodes
        [{:id "root"     :x 0   :y 0   :group nil :index nil :vy nil :vx nil}
         {:id "448e2025" :x 100 :y 100 :group nil :index nil :vy nil :vx nil}
         {:id "448e2066" :x 200 :y 200 :group nil :index nil :vy nil :vx nil}]
        previous-nodes
        [{:id "root"     :x 0   :y 0   :group nil :index nil :vy nil :vx nil}
         {:id "448e2025" :x 100 :y 200 :group nil :index nil :vy nil :vx nil}
         {:id "448e2066" :x 200 :y 300 :group nil :index nil :vy nil :vx nil}]]
    (is
     (=
      (#'c/graph-distance previous-nodes nodes)
      20000))))

(deftest graph-converged?_true
  (let [nodes
        [{:id "root"     :x 0   :y 0   :group nil :index nil :vy nil :vx nil}
         {:id "448e2025" :x 100 :y 100 :group nil :index nil :vy nil :vx nil}]
        previous-nodes nodes]
    (is
     (#'c/graph-converged? 1 previous-nodes nodes))))

(deftest graph-converged?_false
  (let [nodes
        [{:id "root"     :x 0   :y 0   :group nil :index nil :vy nil :vx nil}
         {:id "448e2025" :x 100 :y 100 :group nil :index nil :vy nil :vx nil}]
        previous-nodes
        [{:id "root"     :x 0   :y 0   :group nil :index nil :vy nil :vx nil}
         {:id "448e2025" :x 100 :y 101 :group nil :index nil :vy nil :vx nil}]]
    (is
     (not (#'c/graph-converged? 1 previous-nodes nodes)))))
