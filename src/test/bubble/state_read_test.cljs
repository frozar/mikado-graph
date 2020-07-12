(ns bubble.state-read-test
  (:require
   [bubble.bubble :as b]
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.state :as sd]
   [bubble.state-read :as sr]
   [bubble.state-write :as sw]
   [cljs.test :refer (deftest is)]
   ))

(def appstate-1-bubble
  (#'sd/init-appstate))

(deftest get-bubbles_basic
  (is
   (=
    (-> appstate-1-bubble
        sr/get-bubbles
        keys)
    (list "root")))
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        sr/get-bubbles
        keys)
    (list "root" "bubble-1")))
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-2")
        sr/get-bubbles
        keys)
    (list "root" "bubble-1" "bubble-2"))))

(deftest get-links_basic
  (is
   (=
    (-> appstate-1-bubble
        sr/get-links)
    []))
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        sr/get-links)
    [{:src "root", :dst "bubble-1"}]))
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-2")
        sr/get-links)
    [{:src "root", :dst "bubble-1"}
     {:src "root", :dst "bubble-2"}])))

(deftest connected-graph_1-bubble
  (is
   (=
    (-> appstate-1-bubble
        (sr/connected-graph ROOT-BUBBLE-ID))
    appstate-1-bubble)))

(deftest connected-graph_3-bubble-1-link
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        (#'sw/add-bubble "bubble-2" (b/create-bubble "bubble-2" 0 0))
        (sr/connected-graph ROOT-BUBBLE-ID)
        sr/get-bubbles
        keys
        (#(into #{} %))
        )
    #{"root" "bubble-1"})))

(deftest connected-graph_3-bubble-2-link
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        (#'sw/add-bubble "bubble-2" (b/create-bubble "bubble-2" 0 0))
        (sw/add-link "bubble-2" "bubble-1")
        (sr/connected-graph ROOT-BUBBLE-ID)
        sr/get-links
        )
    [{:src "root", :dst "bubble-1"}])))

(deftest connected-graph_4-bubble-3-link
  (is
   (=
    (-> appstate-1-bubble
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 450 450 "bubble-1")
        (sw/create-bubble-and-link "bubble-1" 350 450 "bubble-2")
        (sw/create-bubble-and-link "bubble-1" 550 450 "bubble-3")
        (sr/connected-graph ROOT-BUBBLE-ID)
        sr/get-bubbles
        keys
        (#(into #{} %))
        )
    #{"root" "bubble-1" "bubble-2" "bubble-3"})))

(def appstate-0-bubble
  (update (#'sd/init-appstate) :bubbles (fn [_] {})))

(deftest graph-barycenter_2-bubble
  (is
   (=
    (-> appstate-0-bubble
        (#'sw/add-bubble ROOT-BUBBLE-ID (b/create-bubble ROOT-BUBBLE-ID 0 0))
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 100 30 "bubble-1")
        sr/graph-barycenter)
    {:x 50 :y 15})))

(deftest graph-barycenter_3-bubble
  (is
   (=
    (-> appstate-0-bubble
        (#'sw/add-bubble ROOT-BUBBLE-ID (b/create-bubble ROOT-BUBBLE-ID 0 0))
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 100 30 "bubble-1")
        (sw/create-bubble-and-link ROOT-BUBBLE-ID 200 60 "bubble-2")
        sr/graph-barycenter)
    {:x 100 :y 30})))
