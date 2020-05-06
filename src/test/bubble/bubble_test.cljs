(ns bubble.bubble-test
  (:require
   [bubble.bubble :as b]
   [bubble.constant :refer (BUBBLE-TYPE)]
   [cljs.test :refer (deftest testing is)]
   ))

(deftest create-bubble_basic
  (let [new-bubble (b/create-bubble "fake-bubble" 20 50)
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
  (let [new-bubble (b/create-bubble "original-id" 20 50)
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

(deftest top-bubble_basic
  (let [new-bubble (b/create-bubble "original-id" 20 50)
        updated-bubble
        (b/update-bubble new-bubble {:ry 20})]
    (testing "Top bubble"
      (is (= (b/top-bubble updated-bubble) 30)))))

(deftest bottom-bubble_basic
  (let [new-bubble (b/create-bubble "original-id" 20 50)
        updated-bubble
        (b/update-bubble new-bubble {:ry 20})]
    (testing "Bottom bubble"
      (is (= (b/bottom-bubble updated-bubble) 70)))))

(deftest left-bubble_basic
  (let [new-bubble (b/create-bubble "original-id" 20 50)
        updated-bubble
        (b/update-bubble new-bubble {:rx 20})]
    (testing "Left bubble"
      (is (= (b/left-bubble updated-bubble) 0)))))

(deftest right-bubble_basic
  (let [new-bubble (b/create-bubble "original-id" 20 50)
        updated-bubble
        (b/update-bubble new-bubble {:rx 20})]
    (testing "Right bubble"
      (is (= (b/right-bubble updated-bubble) 40)))))
