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

(deftest vec-camera_basic
  (let [src-camera initial-camera
        dst-camera (merge src-camera {:cx 800})]
    (is
     (=
      (#'c/vec-camera src-camera dst-camera)
      {:cx 400 :cy 0 :width 0 :height 0 :zoom 0}
      ))))

(deftest add-camera_basic
  (let [src-camera initial-camera
        inc-camera
        {:cx 1 :cy 0 :zoom 0}]
    (is
     (=
      (#'c/add-camera src-camera inc-camera)
      {:cx 401 :cy 300 :width 800 :height 600 :zoom 1}
      ))))

(deftest div-camera_basic
  (let [src-camera initial-camera
        divisor 2]
    (is
     (=
      (#'c/div-camera src-camera divisor)
      {:cx 200 :cy 150 :zoom 0.5}
      ))))

(deftest subpart-camera_basic
  (let [src-camera initial-camera
        set-of-keys #{:cx :cy}]
    (is
     (=
      (#'c/subpart-camera src-camera set-of-keys)
      {:cx 400 :cy 300}
      ))))

(deftest camera-linear-interpolation_basic
  (let [src-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
        dst-camera {:cx 800 :cy 600 :width 800 :height 600 :zoom 2}
        nb-step 3]
    (is
     (=
      (#'c/camera-linear-interpolation src-camera dst-camera nb-step)
      (list {:cx 400, :cy 300, :width 800, :height 600, :zoom 1}
            {:cx 600, :cy 450, :width 800, :height 600, :zoom 1.5}
            {:cx 800, :cy 600, :width 800, :height 600, :zoom 2})
      ))))

(deftest change-coord-px-user-px_basic
  (let [camera initial-camera
        ;; take an arbitrary point
        pt-svg-px [42 24]]
    (is
     (=
      (->> pt-svg-px
           (c/svg-px->svg-user-coord camera)
           (c/svg-user-coord->svg-px camera))
      pt-svg-px
      ))))
