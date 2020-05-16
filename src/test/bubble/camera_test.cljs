(ns bubble.camera-test
  (:require
   [bubble.camera :as c]
   [cljs.test :refer (deftest is)]
   ))

(def initial-camera (c/init-camera 800 600))

(deftest initial-camera_value
  (is
   (=
    initial-camera
    {:cx 400 :cy 300 :width 800 :height 600 :zoom 1})))

(deftest change-coord-px-user-px_basic
  (let [camera initial-camera
        ;; take an arbitrary point
        pt-svg-px [42 24]]
    (is
     (=
      (->> pt-svg-px
           (c/svg-px->svg-user camera)
           (c/svg-user->svg-px camera))
      pt-svg-px
      ))))
