(ns bubble.state-test
  (:require [bubble.state :as s]
            [bubble.bubble :as b]
            [bubble.constant :refer (ROOT-BUBBLE-ID)]
            [cljs.test :refer (deftest testing is)]
            [com.rpl.specter :as sp]
            ))

(deftest init-appstate_basic
  (is
   (=
    (-> (s/init-appstate) :bubbles)
    [b/root-bubble])
   "Is the appstate initialised with the root bubble"))

(deftest add-bubble_basic
  (is
   (=
    (-> (s/init-appstate)
        (s/add-bubble (b/create-bubble 0 0 "fake-bubble"))
        (s/get-bubble "fake-bubble")
        :id)
    "fake-bubble"
    )
   "Is a new bubble 'fake-bubble' exist in the appstate")
  )

(deftest delete-bubble_basic
  (let [new-appstate
        (-> (s/init-appstate)
            (s/add-bubble (b/create-bubble 0 0 "fake-bubble"))
            (s/delete-bubble "fake-bubble"))]
    (is
     (=
      (-> new-appstate :bubbles)
      [b/root-bubble])
     "The only remaining bubble is the root bubble")
    )
  )

(deftest update-bubble_basic
  (is
   (=
    (-> (s/init-appstate)
        (s/update-bubble ROOT-BUBBLE-ID {:cx -10 :cy -20})
        (s/get-bubble ROOT-BUBBLE-ID))
    (b/update-bubble b/root-bubble {:cx -10 :cy -20}))
   "The root bubble is correctly updated")
  )

(deftest create-bubble-and-link_basic
  (let [new-appstate
        (-> (s/init-appstate)
            (s/create-bubble-and-link ROOT-BUBBLE-ID 0 0 "fake-bubble"))]
    (testing "Add and link a new bubble"
      (is
       (=
        (-> new-appstate
            (s/get-bubble "fake-bubble")
            :id)
        "fake-bubble")
       "Is the new bubble 'fake-bubble' is added")
      (is
       (=
        (-> new-appstate
            (s/get-links))
        [{:src ROOT-BUBBLE-ID, :dst "fake-bubble"}]
        )
       "Is the new bubble 'fake-bubble' is linked with the root bubble")
      ))
  (let [new-appstate
        (-> (s/init-appstate)
            (s/create-bubble-and-link ROOT-BUBBLE-ID 0 0 ROOT-BUBBLE-ID))
        list-id
        (-> new-appstate
            (s/get-list-id))]
    (testing "Try to add a bubble with a duplicated id"
      (is
       (= (count list-id) 2)
       "The number of id must be 2")
      (is
       (not=
        list-id
        [ROOT-BUBBLE-ID ROOT-BUBBLE-ID])
       "All id must be different")
      (is
       (=
        (-> new-appstate
            (s/get-links)
            count)
        1
        )
       "A link must be created")
      ))
  )

(def appstate-2-bubble
  (-> (s/init-appstate)
      (s/add-bubble (b/create-bubble 0 0 "fake-bubble")))
  )

(deftest enable-edition_basic
  (let [new-appstate (s/enable-edition appstate-2-bubble ROOT-BUBBLE-ID)]
    (is
     (true? (-> (s/get-bubble new-appstate ROOT-BUBBLE-ID) :edition?)))
    (is
     (false? (-> (s/get-bubble new-appstate "fake-bubble") :edition?)))))
