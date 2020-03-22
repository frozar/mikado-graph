(ns bubble.state-test
  (:require [bubble.state :as s]
            [cljs.test :refer (deftest is)]
            ))

(deftest init-appstate_basic
  (is
   (=
    (-> (s/init-appstate) :bubbles)
    [s/root-bubble])))

(def appstate-2-bubble
  (-> (s/init-appstate)
      (s/add-bubble (s/get-new-bubble 0 0 "fake-bubble")))
  )

(deftest add-bubble_basic
  (is
   (=
    (-> appstate-2-bubble
        (s/get-bubble "fake-bubble")
        :id)
    "fake-bubble"
    )))

(deftest enable-edition_basic
  (let [new-appstate (s/enable-edition appstate-2-bubble "root")]
    (is
     (true? (-> (s/get-bubble new-appstate "root") :edition?)))
    (is
     (false? (-> (s/get-bubble new-appstate "fake-bubble") :edition?)))))
