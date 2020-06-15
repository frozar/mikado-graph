(ns bubble.state-write-test
  (:require
   [bubble.bubble :as b]
   [bubble.constant :refer [ROOT-BUBBLE-ID]]
   [bubble.state :as sd]
   [bubble.state-read :as sr]
   [bubble.state-write :as sw]
   [cljs.test :refer (deftest is)]
   ))

(deftest init-appstate_basic
  (is
   (=
    (-> (#'sd/init-appstate) :bubbles)
    {ROOT-BUBBLE-ID b/root-bubble})
   "Is the appstate initialised with the root bubble"))

(deftest add-bubble_basic
  (let [fake-bubble (b/create-bubble "fake-bubble" 0 0)]
    (is
     (=
      (-> (#'sd/init-appstate)
          (#'sw/add-bubble "fake-bubble" fake-bubble)
          (sr/get-bubble "fake-bubble"))
      fake-bubble
      )
     "Is a new bubble 'fake-bubble' exist in the appstate"))
  )

(deftest delete-bubble_basic
  (let [new-appstate
        (-> (#'sd/init-appstate)
            (#'sw/add-bubble "fake-bubble" (b/create-bubble "fake-bubble" 0 0))
            (#'sw/delete-bubble "fake-bubble"))]
    (is
     (map? (-> new-appstate :bubbles))
     "The 'bubbles' collection remain a map")
    (is
     (=
      (-> new-appstate :bubbles)
      {ROOT-BUBBLE-ID b/root-bubble} #_[b/root-bubble])
     "The only remaining bubble is the root bubble")
    )
  )

(deftest update-bubble_basic
  (is
   (=
    (-> (#'sd/init-appstate)
        (#'sw/update-bubble ROOT-BUBBLE-ID {:cx -10 :cy -20})
        (sr/get-bubble ROOT-BUBBLE-ID))
    (b/update-bubble b/root-bubble {:cx -10 :cy -20}))
   "The root bubble is correctly updated")
  )

(def appstate-2-bubble
  (-> (#'sd/init-appstate)
      (#'sw/add-bubble "bubble-1" (b/create-bubble "bubble-1" 0 0)))
  )

(deftest add-link_basic
  (let [new-appstate (sw/add-link appstate-2-bubble ROOT-BUBBLE-ID "bubble-1")]
    (is
     (not=
      (some #{{:src ROOT-BUBBLE-ID :dst "bubble-1"}} (sr/get-links new-appstate))
      nil
      )
     "There is a link between the 'root' bubble and 'fake-bubble'"
     )
    )
  (let [new-appstate
        (-> appstate-2-bubble
            (sw/add-link ROOT-BUBBLE-ID ROOT-BUBBLE-ID))]
    (is
     (= (count (sr/get-links new-appstate)) 0)
     "A link must be between 2 distincts bubbles"
     )
    )
  (let [new-appstate
        (-> appstate-2-bubble
            (sw/add-link ROOT-BUBBLE-ID "bubble-1")
            (sw/add-link ROOT-BUBBLE-ID "bubble-1"))]
    (is
     (= (count (sr/get-links new-appstate)) 1)
     "A link cannot be duplicated"
     )
    )
  (let [new-appstate
        (-> appstate-2-bubble
            (sw/add-link ROOT-BUBBLE-ID "inexistant-bubble"))]
    (is
     (= (count (sr/get-links new-appstate)) 0)
     "Cannot add a link not associated with a bubble-id"
     )
    )
  (let [new-appstate
        (-> appstate-2-bubble
            (sw/add-link ROOT-BUBBLE-ID "bubble-1")
            (sw/add-link "bubble-1" ROOT-BUBBLE-ID))]
    (is
     (= (count (sr/get-links new-appstate)) 1)
     "Cannot link 2 bubbles in both directions"
     )
    )
  )

(deftest add-links_basic
  (let [new-links [{:src ROOT-BUBBLE-ID :dst "bubble-1"}
                   {:src ROOT-BUBBLE-ID :dst "inexistant-bubble"}]
        new-appstate
        (-> appstate-2-bubble
            (sw/add-links new-links))]
    (is
     (= (count (sr/get-links new-appstate)) 1)
     "The link 'root' -> 'bubble-1' but not the 'root' -> 'inexistant-bubble'"
     )
    (is
     (true? (sr/link-exist new-appstate ROOT-BUBBLE-ID "bubble-1"))
     )
    )
  )

(deftest enable-edition_basic
  (let [new-appstate (#'sw/enable-edition appstate-2-bubble ROOT-BUBBLE-ID)]
    (is
     (true? (-> (sr/get-bubble new-appstate ROOT-BUBBLE-ID) :edition?)))
    (is
     (false? (-> (sr/get-bubble new-appstate "bubble-1") :edition?)))))

(deftest disable-edition_basic
  (let [new-appstate
        (-> appstate-2-bubble
            (#'sw/enable-edition ROOT-BUBBLE-ID)
            (#'sw/enable-edition "bubble-1")
            (#'sw/disable-edition ROOT-BUBBLE-ID))]
    (is
     (false? (-> (sr/get-bubble new-appstate ROOT-BUBBLE-ID) :edition?)))
    (is
     (true? (-> (sr/get-bubble new-appstate "bubble-1") :edition?)))))

(deftest toggle-done-status_basic
  (let [new-appstate
        (-> appstate-2-bubble
            (#'sw/toggle-done-status ROOT-BUBBLE-ID))]
    (is
     (true? (-> (sr/get-bubble new-appstate ROOT-BUBBLE-ID) :done?)))
    )
  (let [new-appstate
        (-> appstate-2-bubble
            (#'sw/toggle-done-status ROOT-BUBBLE-ID)
            (#'sw/toggle-done-status ROOT-BUBBLE-ID))]
    (is
     (false? (-> (sr/get-bubble new-appstate ROOT-BUBBLE-ID) :done?)))
    )
  )

(deftest create-bubble-and-link_basic
  (let [new-appstate
        (-> (#'sd/init-appstate)
            (#'sw/create-bubble-and-link ROOT-BUBBLE-ID 50 50 "bubble-1"))
        ]
    (is
     (= (count (sr/get-bubbles new-appstate)) 2)
     "2 bubbles in the graph"
     )
    (is
     (= ((sr/get-bubble new-appstate ROOT-BUBBLE-ID) :id) ROOT-BUBBLE-ID)
     "There is a 'root' bubble"
     )
    (is
     (= ((sr/get-bubble new-appstate "bubble-1") :id) "bubble-1")
     "There is a 'bubble-1' bubble"
     )
    (is
     (= (count (sr/get-links new-appstate)) 1)
     "1 link is added"
     )
    (is
     (not=
      (some #{{:src ROOT-BUBBLE-ID :dst "bubble-1"}} (sr/get-links new-appstate))
      nil)
     "There is a link between 'root' bubble and 'bubble-1'"
     )
    )
  )

(def appstate-3-bubble-3-link
  (-> (#'sd/init-appstate)
      (#'sw/create-bubble-and-link ROOT-BUBBLE-ID 50 50 "bubble-1")
      (#'sw/create-bubble-and-link "bubble-1" 350 350 "bubble-2")
      (#'sw/add-link ROOT-BUBBLE-ID "bubble-2")
      )
  )

(deftest delete-link_basic
  (let [new-appstate
        (#'sw/delete-link
         appstate-3-bubble-3-link "bubble-1" "bubble-2")
        ]
    (is
     (= (count (sr/get-bubbles new-appstate)) 3)
     "3 bubbles in the graph"
     )
    (is
     (and
      (= ((sr/get-bubble new-appstate ROOT-BUBBLE-ID) :id) ROOT-BUBBLE-ID)
      (= ((sr/get-bubble new-appstate "bubble-1") :id) "bubble-1")
      (= ((sr/get-bubble new-appstate "bubble-2") :id) "bubble-2")
      )
     "The bubbles id are good"
     )
    (is
     (= (count (sr/get-links new-appstate)) 2)
     "1 link is added"
     )
    (is
     (=
      (some #{{:src "bubble-1" :dst "bubble-2"}} (sr/get-links new-appstate))
      nil)
     "The link between 'bubble-1' and 'bubble-2' has been deleted"
     )
    )
  )

(def appstate-6-bubble-6-link
  (-> (#'sd/init-appstate)
      (#'sw/create-bubble-and-link ROOT-BUBBLE-ID 50 50 "bubble-1")
      (#'sw/create-bubble-and-link ROOT-BUBBLE-ID 150 150 "bubble-2")
      (#'sw/create-bubble-and-link "bubble-1" 350 350 "bubble-3")
      (sw/add-link "bubble-2" "bubble-3")
      (#'sw/create-bubble-and-link "bubble-3" 450 550 "bubble-4")
      (#'sw/create-bubble-and-link "bubble-3" 450 550 "bubble-5")
      )
  )

(deftest delete-link-involving-bubble_basic
  (let [new-appstate
        (#'sw/delete-link-to-id-and-update-children-of-id
         appstate-6-bubble-6-link "bubble-3")
        ]
    (is
     (= (count (sr/get-bubbles new-appstate)) 6)
     "6 bubbles in the graph"
     )
    (is
     (and
      (sr/bubble-id-exist new-appstate ROOT-BUBBLE-ID)
      (sr/bubble-id-exist new-appstate "bubble-1")
      (sr/bubble-id-exist new-appstate "bubble-2")
      (sr/bubble-id-exist new-appstate "bubble-3")
      (sr/bubble-id-exist new-appstate "bubble-4")
      (sr/bubble-id-exist new-appstate "bubble-5")
      )
     "The bubbles id are good"
     )
    (is
     (= (count (sr/get-links new-appstate)) 6)
     "6 links remains"
     )
    (is
     (and
      (true? (sr/link-exist new-appstate ROOT-BUBBLE-ID "bubble-1"))
      (true? (sr/link-exist new-appstate ROOT-BUBBLE-ID "bubble-2"))
      (true? (sr/link-exist new-appstate "bubble-1" "bubble-4"))
      (true? (sr/link-exist new-appstate "bubble-1" "bubble-5"))
      (true? (sr/link-exist new-appstate "bubble-2" "bubble-4"))
      (true? (sr/link-exist new-appstate "bubble-2" "bubble-5"))
      )
     "There is a link between 'root' bubble and 'bubble-2'"
     )
    )
  )

(deftest delete-bubble-and-update-link_basic
  (let [new-appstate
        (#'sw/delete-bubble-and-update-link
         appstate-6-bubble-6-link "bubble-3")
        ]
    (is
     (= (count (sr/get-bubbles new-appstate)) 5)
     "5 bubbles in the graph"
     )
    (is
     (and
      (sr/bubble-id-exist new-appstate ROOT-BUBBLE-ID)
      (sr/bubble-id-exist new-appstate "bubble-1")
      (sr/bubble-id-exist new-appstate "bubble-2")
      (sr/bubble-id-exist new-appstate "bubble-4")
      (sr/bubble-id-exist new-appstate "bubble-5")
      )
     "The bubbles id are good"
     )
    (is
     (= (count (sr/get-links new-appstate)) 6)
     "6 links remains"
     )
    (is
     (and
      (true? (sr/link-exist new-appstate ROOT-BUBBLE-ID "bubble-1"))
      (true? (sr/link-exist new-appstate ROOT-BUBBLE-ID "bubble-2"))
      (true? (sr/link-exist new-appstate "bubble-1" "bubble-4"))
      (true? (sr/link-exist new-appstate "bubble-1" "bubble-5"))
      (true? (sr/link-exist new-appstate "bubble-2" "bubble-4"))
      (true? (sr/link-exist new-appstate "bubble-2" "bubble-5"))
      )
     "There is a link between 'root' bubble and 'bubble-2'"
     )
    )
  )

(deftest move-bubble_basic
  (let [new-state
        (#'sw/move-bubble appstate-2-bubble ROOT-BUBBLE-ID 100 -100)
        {:keys [cx cy]} (sr/get-bubble new-state ROOT-BUBBLE-ID)]
    (is (= cx 100))
    (is (= cy -100))))

(deftest move-bubbles_basic
  (let [new-state
        (#'sw/move-bubbles appstate-2-bubble
                           {ROOT-BUBBLE-ID {:cx 100 :cy -100}
                            "bubble-1" {:cx 200 :cy -200}})
        {cx0 :cx cy0 :cy} (sr/get-bubble new-state ROOT-BUBBLE-ID)
        {cx1 :cx cy1 :cy} (sr/get-bubble new-state "bubble-1")]
    (is (= cx0 100))
    (is (= cy0 -100))
    (is (= cx1 200))
    (is (= cy1 -200))))
