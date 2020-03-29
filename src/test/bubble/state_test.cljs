(ns bubble.state-test
  (:require [bubble.state :as s]
            [bubble.bubble :as b]
            [bubble.constant :refer (ROOT-BUBBLE-ID)]
            [cljs.test :refer (deftest is)] ;; testing
            ))

(deftest init-appstate_basic
  (is
   (=
    (-> (#'s/init-appstate) :bubbles)
    [b/root-bubble])
   "Is the appstate initialised with the root bubble"))

(deftest add-bubble_basic
  (is
   (=
    (-> (#'s/init-appstate)
        (#'s/add-bubble (b/create-bubble 0 0 "fake-bubble"))
        (s/get-bubble "fake-bubble")
        :id)
    "fake-bubble"
    )
   "Is a new bubble 'fake-bubble' exist in the appstate")
  )

(deftest delete-bubble_basic
  (let [new-appstate
        (-> (#'s/init-appstate)
            (#'s/add-bubble (b/create-bubble 0 0 "fake-bubble"))
            (#'s/delete-bubble "fake-bubble"))]
    (is
     (vector? (-> new-appstate :bubbles))
     "The 'bubbles' collection remain a vector")
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
    (-> (#'s/init-appstate)
        (#'s/update-bubble ROOT-BUBBLE-ID {:cx -10 :cy -20})
        (s/get-bubble ROOT-BUBBLE-ID))
    (b/update-bubble b/root-bubble {:cx -10 :cy -20}))
   "The root bubble is correctly updated")
  )

(def appstate-2-bubble
  (-> (#'s/init-appstate)
      (#'s/add-bubble (b/create-bubble 0 0 "bubble-1")))
  )

(deftest add-link_basic
  (let [new-appstate (s/add-link appstate-2-bubble ROOT-BUBBLE-ID "bubble-1")]
    (is
     (not=
      (some #{{:src ROOT-BUBBLE-ID :dst "bubble-1"}} (s/get-links new-appstate))
      nil
      )
     "There is a link between the 'root' bubble and 'fake-bubble'"
     )
    )
  (let [new-appstate
        (-> appstate-2-bubble
            (s/add-link ROOT-BUBBLE-ID "bubble-1")
            (s/add-link ROOT-BUBBLE-ID "bubble-1"))]
    (is
     (= (count (s/get-links new-appstate)) 1)
     "A link cannot be duplicated"
     )
    )
  (let [new-appstate
        (-> appstate-2-bubble
            (s/add-link ROOT-BUBBLE-ID "inexistant-bubble"))]
    (is
     (= (count (s/get-links new-appstate)) 0)
     "Cannot add a link not associated with a bubble-id"
     )
    )
  )

(deftest add-links_basic
  (let [new-links [{:src ROOT-BUBBLE-ID :dst "bubble-1"}
                   {:src ROOT-BUBBLE-ID :dst "inexistant-bubble"}]
        new-appstate
        (-> appstate-2-bubble
            (s/add-links new-links))]
    (is
     (= (count (s/get-links new-appstate)) 1)
     "The link 'root' -> 'bubble-1' but not the 'root' -> 'inexistant-bubble'"
     )
    (is
     (true? (s/link-exist new-appstate ROOT-BUBBLE-ID "bubble-1"))
     )
    )
  )

(deftest enable-edition_basic
  (let [new-appstate (#'s/enable-edition appstate-2-bubble ROOT-BUBBLE-ID)]
    (is
     (true? (-> (s/get-bubble new-appstate ROOT-BUBBLE-ID) :edition?)))
    (is
     (false? (-> (s/get-bubble new-appstate "bubble-1") :edition?)))))

(deftest disable-edition_basic
  (let [new-appstate
        (-> appstate-2-bubble
            (#'s/enable-edition ROOT-BUBBLE-ID)
            (#'s/enable-edition "bubble-1")
            (#'s/disable-edition ROOT-BUBBLE-ID))]
    (is
     (false? (-> (s/get-bubble new-appstate ROOT-BUBBLE-ID) :edition?)))
    (is
     (true? (-> (s/get-bubble new-appstate "bubble-1") :edition?)))))

(deftest enable-show-button_basic
  (let [new-appstate (#'s/enable-show-button appstate-2-bubble ROOT-BUBBLE-ID)]
    (is
     (true? (-> (s/get-bubble new-appstate ROOT-BUBBLE-ID) :show-button?)))
    (is
     (false? (-> (s/get-bubble new-appstate "bubble-1") :show-button?)))))

(deftest disable-show-button_basic
  (let [new-appstate
        (-> appstate-2-bubble
            (#'s/enable-show-button ROOT-BUBBLE-ID)
            (#'s/enable-show-button "bubble-1")
            (#'s/disable-show-button ROOT-BUBBLE-ID))]
    (is
     (false? (-> (s/get-bubble new-appstate ROOT-BUBBLE-ID) :show-button?)))
    (is
     (true? (-> (s/get-bubble new-appstate "bubble-1") :show-button?)))))

(deftest toggle-done-status_basic
  (let [new-appstate
        (-> appstate-2-bubble
            (#'s/toggle-done-status ROOT-BUBBLE-ID))]
    (is
     (true? (-> (s/get-bubble new-appstate ROOT-BUBBLE-ID) :done?)))
    )
  (let [new-appstate
        (-> appstate-2-bubble
            (#'s/toggle-done-status ROOT-BUBBLE-ID)
            (#'s/toggle-done-status ROOT-BUBBLE-ID))]
    (is
     (false? (-> (s/get-bubble new-appstate ROOT-BUBBLE-ID) :done?)))
    )
  )

(deftest create-bubble-and-link_basic
  (let [new-appstate
        (-> (#'s/init-appstate)
            (#'s/create-bubble-and-link ROOT-BUBBLE-ID 50 50 "bubble-1"))
        ]
    (is
     (= (count (s/get-bubbles new-appstate)) 2)
     "2 bubbles in the graph"
     )
    (is
     (= ((s/get-bubble new-appstate ROOT-BUBBLE-ID) :id) ROOT-BUBBLE-ID)
     "There is a 'root' bubble"
     )
    (is
     (= ((s/get-bubble new-appstate "bubble-1") :id) "bubble-1")
     "There is a 'bubble-1' bubble"
     )
    (is
     (= (count (s/get-links new-appstate)) 1)
     "1 link is added"
     )
    (is
     (not=
      (some #{{:src ROOT-BUBBLE-ID :dst "bubble-1"}} (s/get-links new-appstate))
      nil)
     "There is a link between 'root' bubble and 'bubble-1'"
     )
    )
  )

(def appstate-3-bubble-3-link
  (-> (#'s/init-appstate)
      (#'s/create-bubble-and-link ROOT-BUBBLE-ID 50 50 "bubble-1")
      (#'s/create-bubble-and-link "bubble-1" 350 350 "bubble-2")
      (#'s/add-link ROOT-BUBBLE-ID "bubble-2")
      )
  )

(deftest delete-link_basic
  (let [new-appstate
        (#'s/delete-link
         appstate-3-bubble-3-link "bubble-1" "bubble-2")
        ]
    (is
     (= (count (s/get-bubbles new-appstate)) 3)
     "3 bubbles in the graph"
     )
    (is
     (and
      (= ((s/get-bubble new-appstate ROOT-BUBBLE-ID) :id) ROOT-BUBBLE-ID)
      (= ((s/get-bubble new-appstate "bubble-1") :id) "bubble-1")
      (= ((s/get-bubble new-appstate "bubble-2") :id) "bubble-2")
      )
     "The bubbles id are good"
     )
    (is
     (= (count (s/get-links new-appstate)) 2)
     "1 link is added"
     )
    (is
     (=
      (some #{{:src "bubble-1" :dst "bubble-2"}} (s/get-links new-appstate))
      nil)
     "The link between 'bubble-1' and 'bubble-2' has been deleted"
     )
    )
  )

(def appstate-6-bubble-6-link
  (-> (#'s/init-appstate)
      (#'s/create-bubble-and-link ROOT-BUBBLE-ID 50 50 "bubble-1")
      (#'s/create-bubble-and-link ROOT-BUBBLE-ID 150 150 "bubble-2")
      (#'s/create-bubble-and-link "bubble-1" 350 350 "bubble-3")
      (s/add-link "bubble-2" "bubble-3")
      (#'s/create-bubble-and-link "bubble-3" 450 550 "bubble-4")
      (#'s/create-bubble-and-link "bubble-3" 450 550 "bubble-5")
      )
  )

(deftest delete-link-involving-bubble_basic
  (let [new-appstate
        (#'s/delete-link-to-id-and-update-children-of-id
         appstate-6-bubble-6-link "bubble-3")
        ]
    (is
     (= (count (s/get-bubbles new-appstate)) 6)
     "6 bubbles in the graph"
     )
    (is
     (and
      (s/bubble-id-exist new-appstate ROOT-BUBBLE-ID)
      (s/bubble-id-exist new-appstate "bubble-1")
      (s/bubble-id-exist new-appstate "bubble-2")
      (s/bubble-id-exist new-appstate "bubble-3")
      (s/bubble-id-exist new-appstate "bubble-4")
      (s/bubble-id-exist new-appstate "bubble-5")
      )
     "The bubbles id are good"
     )
    (is
     (= (count (s/get-links new-appstate)) 6)
     "6 links remains"
     )
    (is
     (and
      (true? (s/link-exist new-appstate ROOT-BUBBLE-ID "bubble-1"))
      (true? (s/link-exist new-appstate ROOT-BUBBLE-ID "bubble-2"))
      (true? (s/link-exist new-appstate "bubble-1" "bubble-4"))
      (true? (s/link-exist new-appstate "bubble-1" "bubble-5"))
      (true? (s/link-exist new-appstate "bubble-2" "bubble-4"))
      (true? (s/link-exist new-appstate "bubble-2" "bubble-5"))
      )
     "There is a link between 'root' bubble and 'bubble-2'"
     )
    )
  )

(deftest delete-bubble-and-update-link_basic
  (let [new-appstate
        (#'s/delete-bubble-and-update-link
         appstate-6-bubble-6-link "bubble-3")
        ]
    (is
     (= (count (s/get-bubbles new-appstate)) 5)
     "5 bubbles in the graph"
     )
    (is
     (and
      (s/bubble-id-exist new-appstate ROOT-BUBBLE-ID)
      (s/bubble-id-exist new-appstate "bubble-1")
      (s/bubble-id-exist new-appstate "bubble-2")
      (s/bubble-id-exist new-appstate "bubble-4")
      (s/bubble-id-exist new-appstate "bubble-5")
      )
     "The bubbles id are good"
     )
    (is
     (= (count (s/get-links new-appstate)) 6)
     "6 links remains"
     )
    (is
     (and
      (true? (s/link-exist new-appstate ROOT-BUBBLE-ID "bubble-1"))
      (true? (s/link-exist new-appstate ROOT-BUBBLE-ID "bubble-2"))
      (true? (s/link-exist new-appstate "bubble-1" "bubble-4"))
      (true? (s/link-exist new-appstate "bubble-1" "bubble-5"))
      (true? (s/link-exist new-appstate "bubble-2" "bubble-4"))
      (true? (s/link-exist new-appstate "bubble-2" "bubble-5"))
      )
     "There is a link between 'root' bubble and 'bubble-2'"
     )
    )
  )
