(ns camera.state-test
  (:require
   [camera.state :as s]
   [cljs.test :refer (deftest is)]
   ))

(deftest initial-camera_value
  (is
   (=
    (s/init-camera 800 600)
    {:cx 400 :cy 300 :width 800 :height 600 :zoom 1})))
