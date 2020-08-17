(ns camera.core-test
  (:require
   [camera.core :as c]
   [camera.state :as s]
   [cljs.test :refer (deftest is)]
   ))

(def initial-camera (s/init-camera 800 600))

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
