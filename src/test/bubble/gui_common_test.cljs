(ns bubble.gui-common-test
  (:require
   [bubble.gui-common :as g]
   [cljs.test :refer (deftest testing is)]
   ))

(deftest float-euclidien-div_basic
  (testing "Float euclidien division"
    (is (= (#'g/float-euclidien-div 5.5 2.5) 0.5))
    (is (= (#'g/float-euclidien-div -5.5 2.5) 2.0))
    ))

(deftest get-relative-th0_basic
  (testing "Relative incident angle on bubble"
    (is (= (#'g/get-relative-th0 (- 0 js/Math.PI) :source) (- 0 js/Math.PI)))
    (is (= (#'g/get-relative-th0 0 :source) 0))
    (is (= (#'g/get-relative-th0 (/ js/Math.PI 2) :source) (/ js/Math.PI 2)))
    (is (= (#'g/get-relative-th0 js/Math.PI :source) (- 0 js/Math.PI)))

    (is (= (#'g/get-relative-th0 (- 0 js/Math.PI) :target) 0))
    (is (= (#'g/get-relative-th0 0 :target) (- 0 js/Math.PI)))
    (is (= (#'g/get-relative-th0 js/Math.PI :target) 0))
    ))

(deftest angle-between-bubbles_basic
  (testing "Angle between bubbles"
    (let [src-b {:cx 0 :cy 0}
          dst-b {:cx 100 :cy 0}]
      (is (=
           (g/angle-between-bubbles src-b dst-b)
           0)))
    (let [src-b {:cx 0 :cy 0}
          dst-b {:cx 100 :cy 100}]
      (is (=
           (g/angle-between-bubbles src-b dst-b)
           (/ js/Math.PI 4))))
    (let [src-b {:cx 0 :cy 0}
          dst-b {:cx 0 :cy 100}]
      (is (=
           (g/angle-between-bubbles src-b dst-b)
           (/ js/Math.PI 2))))))

(deftest border-point_basic
  (testing "Border ellipse point"
    (let [src-b {:cx 0 :cy 0 :rx 2 :ry 1}
          dst-b {:cx 100 :cy 100 :rx 2 :ry 1}]
      (is
       (=
        (for [i (range 8)]
          (g/border-point src-b (- (* i (/ (* 2 js/Math.PI) 8)) js/Math.PI) :source))
        (list [-2 -3.216245299353273e-16]
              [-0.8944271909999163 -0.8944271909999157]
              [1.2246467991473532e-16 -1]
              [0.8944271909999161 -0.8944271909999159]
              [2 0]
              [0.8944271909999161 0.8944271909999159]
              [1.2246467991473532e-16 1]
              [-0.8944271909999159 0.8944271909999159])
        ))
      (is
       (=
        (for [i (range 8)]
          (g/border-point dst-b (* i (/ (* 2 js/Math.PI) 8)) :target))
        (list [98 100]
              [99.10557280900008 99.10557280900008]
              [100 99]
              [100.89442719099992 99.10557280900008]
              [102 100]
              [100.89442719099992 100.89442719099992]
              [100 101]
              [99.10557280900008 100.89442719099992])
        ))
      (is
       (=
        (for [i (range 8)]
          (g/border-point dst-b (- (* i (/ (* 2 js/Math.PI) 8)) js/Math.PI) :target))
        (list [102 100]
              [100.89442719099992 100.89442719099992]
              [100 101]
              [99.10557280900008 100.89442719099992]
              [98 100]
              [99.10557280900008 99.10557280900008]
              [100 99]
              [100.89442719099992 99.10557280900008]
              )
        )))
    ))
