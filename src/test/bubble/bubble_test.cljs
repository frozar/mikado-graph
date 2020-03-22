(ns bubble.bubble-test
  (:require [bubble.bubble :as b]
            [bubble.constant :refer (ROOT-BUBBLE-ID BUBBLE-TYPE)]
            [cljs.test :refer (deftest testing is)]
            ))

(deftest create-bubble_basic
  (let [new-bubble (b/create-bubble 20 50 "fake-bubble")
        {new-id :id
         new-type :type
         new-cx :cx
         new-cy :cy} new-bubble]
    (testing "New bubble"
      (is (= new-id "fake-bubble")
          "Check id")
      (is (= new-type BUBBLE-TYPE)
          "Check type")
      (is (= new-cx 20)
          "Check cx")
      (is (= new-cy 50)
          "Check type")
      )))

(deftest update-bubble_basic
  (let [new-bubble (b/create-bubble 20 50 "original-id")
        updated-bubble
        (b/update-bubble new-bubble {:id "fake-bubble-again"
                                     :type "fake-type"
                                     :cx 666
                                     :cy 777})
        {update-id :id
         update-type :type
         update-cx :cx
         update-cy :cy} updated-bubble]
    (testing "Update bubble"
      (is (= update-id "original-id")
          "Id must remain the same")
      (is (= update-type "fake-type")
          "Check type")
      (is (= update-cx 666)
          "Check cx")
      (is (= update-cy 777)
          "Check type")
    )))
