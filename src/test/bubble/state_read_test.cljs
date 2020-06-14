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
