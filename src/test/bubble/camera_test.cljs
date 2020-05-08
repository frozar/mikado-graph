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
           (c/svg-px->svg-user-coord camera)
           (c/svg-user-coord->svg-px camera))
      pt-svg-px
      ))))

(defn- pt-dist
  [[x0 y0] [x1 y1]]
  (let [vx (- x1 x0)
        vy (- y1 y0)]
    (.sqrt js/Math (+ (* vx vx) (* vy vy)))))

(deftest compute-fix-point-svg-user_basic
  (let [camera0 {:cx 400, :cy 300, :width 800, :height 600, :zoom 1}
        camera1 {:cx 800, :cy 600, :width 800, :height 600, :zoom 2}
        fix-point-svg-user (#'c/compute-fix-point-svg-user camera0 camera1)
        pt-in-camera0 (c/svg-user-coord->svg-px camera0 fix-point-svg-user)
        pt-in-camera1 (c/svg-user-coord->svg-px camera1 fix-point-svg-user)
        ]
    (is
     (<
      (pt-dist pt-in-camera0 pt-in-camera1)
      10e-4
      ))
    ))

(deftest range-linear_basic
  (is
   (=
    (#'c/range-math 0 10 5 :linear)
    (list 0 2.5 5 7.5 10)
    ))
  (is
   (=
    (#'c/range-math 0 -10 5 :linear)
    (list 0 -2.5 -5 -7.5 -10)
    )))

(deftest range-log_basic
  (is
   (=
    (#'c/range-math 0 10 4 :log)
    (list 0 5 7.92481250360578 10)
    ))
  (is
   (=
    (#'c/range-math 0 -10 4 :log)
    (list 0 -5 -7.92481250360578 -10)
    )))

(deftest camera-linear-interpolation-translation_basic
  (let [src-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
        dst-camera {:cx 800 :cy 600 :width 800 :height 600 :zoom 1}
        nb-step 3]
    (is
     (=
      (#'c/camera-interpolation-translation src-camera dst-camera nb-step :linear)
      (list {:cx 400, :cy 300, :width 800, :height 600, :zoom 1}
            {:cx 600, :cy 450, :width 800, :height 600, :zoom 1}
            {:cx 800, :cy 600, :width 800, :height 600, :zoom 1})
      ))))

(deftest camera-linear-interpolation-homothety_basic
  (let [src-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
        dst-camera {:cx 800 :cy 600 :width 800 :height 600 :zoom 2}
        nb-step 3]
    (is
     (=
      (#'c/camera-interpolation-homothety src-camera dst-camera nb-step :linear)
      (list {:cx 400, :cy 300, :width 800, :height 600, :zoom 1}
            {:cx 666.6666666666667, :cy 500, :width 800, :height 600, :zoom 1.5}
            {:cx 800, :cy 600, :width 800, :height 600, :zoom 2})
      ))))

(deftest camera-log-interpolation-translation_basic
  (let [src-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
        dst-camera {:cx 800 :cy 600 :width 800 :height 600 :zoom 1}
        nb-step 3]
    (is
     (=
      (#'c/camera-interpolation-translation src-camera dst-camera nb-step :log)
      (list {:cx 400, :cy 300, :width 800, :height 600, :zoom 1}
            {:cx 652.371901428583, :cy 489.27892607143724, :width 800, :height 600, :zoom 1}
            {:cx 800, :cy 600, :width 800, :height 600, :zoom 1})
      ))))

(deftest camera-log-interpolation-homothety_basic
  (let [src-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
        dst-camera {:cx 800 :cy 600 :width 800 :height 600 :zoom 2}
        nb-step 3]
    (is
     (=
      (#'c/camera-interpolation-homothety src-camera dst-camera nb-step :log)
      (list {:cx 400, :cy 300, :width 800, :height 600, :zoom 1}
            {:cx 709.4822457876334, :cy 532.1116843407249, :width 800, :height 600, :zoom 1.6309297535714575}
            {:cx 799.9999999999999,, :cy 600, :width 800, :height 600, :zoom 2})
      ))))
