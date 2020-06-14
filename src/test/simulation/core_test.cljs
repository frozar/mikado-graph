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
    (c/appstate->graph appstate-1-bubble)
    {:nodes [{:id ROOT-BUBBLE-ID, :x 450, :y 450 :group 1}]
     :links []}))
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        c/appstate->graph)
    {:nodes [{:id ROOT-BUBBLE-ID, :x 450, :y 450 :group 1}
             {:id "bubble-1", :x 450, :y 450, :group 1}]
     :links [{:source ROOT-BUBBLE-ID, :target "bubble-1", :value 10}]}))
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-2")
        c/appstate->graph)
    {:nodes [{:id ROOT-BUBBLE-ID, :x 450, :y 450 :group 1}
             {:id "bubble-1", :x 450, :y 450, :group 1}
             {:id "bubble-2", :x 450, :y 450, :group 1}]
     :links [{:source ROOT-BUBBLE-ID, :target "bubble-1", :value 10}
             {:source ROOT-BUBBLE-ID, :target "bubble-2", :value 10}]})))
