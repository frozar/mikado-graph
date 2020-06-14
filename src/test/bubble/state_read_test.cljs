(ns bubble.state-read-test
  (:require
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.state :as sd]
   [bubble.state-read :as sr]
   [bubble.state-write :as sw]
   [cljs.test :refer (deftest is)]
   ))

(def appstate-1-bubble
  (#'sd/init-appstate)
  )

(deftest appstate->graph_basic
  (is
   (=
    (sr/appstate->graph appstate-1-bubble)
    {:nodes [{:id ROOT-BUBBLE-ID, :x 450, :y 450 :group 1}]
     :links []}))
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        sr/appstate->graph)
    {:nodes [{:id ROOT-BUBBLE-ID, :x 450, :y 450 :group 1}
             {:id "bubble-1", :x 450, :y 450, :group 1}]
     :links [{:source ROOT-BUBBLE-ID, :target "bubble-1", :value 10}]}))
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-2")
        sr/appstate->graph)
    {:nodes [{:id ROOT-BUBBLE-ID, :x 450, :y 450 :group 1}
             {:id "bubble-1", :x 450, :y 450, :group 1}
             {:id "bubble-2", :x 450, :y 450, :group 1}]
     :links [{:source ROOT-BUBBLE-ID, :target "bubble-1", :value 10}
             {:source ROOT-BUBBLE-ID, :target "bubble-2", :value 10}]})))
