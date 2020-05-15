(ns bubble.camera-test
  (:require
   [bubble.coordinate :as coord]
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

(defn- pt-dist
  [[x0 y0] [x1 y1]]
  (let [vx (- x1 x0)
        vy (- y1 y0)]
    (.sqrt js/Math (+ (* vx vx) (* vy vy)))))

(deftest compute-fix-point-svg-user_basic
  (let [camera0 {:cx 400, :cy 300, :width 800, :height 600, :zoom 1}
        camera1 {:cx 800, :cy 600, :width 800, :height 600, :zoom 2}
        fix-point-svg-user (#'c/compute-fix-point-svg-user camera0 camera1)
        pt-in-camera0 (c/svg-user->svg-px camera0 fix-point-svg-user)
        pt-in-camera1 (c/svg-user->svg-px camera1 fix-point-svg-user)
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
    ))
  (is
   (=
    (#'c/range-math 0 1 5 :linear)
    (list 0 0.25 0.5 0.75 1)
    )))

(deftest range-log_basic
  (is
   (=
    (#'c/range-math 0 10 4 :log)
    (list 0 6.020599913279623 8.450980400142567 10)
    ))
  (is
   (=
    (#'c/range-math 0 -10 4 :log)
    (list 0 -6.020599913279623 -8.450980400142567 -10)
    ))
  (is
   (=
    (#'c/range-math 0 20 4 :log)
    (list 0 12.041199826559247 16.901960800285135 20)
    )))

(deftest range-reverse-gaussian_basic
  (is
   (=
    (#'c/range-math 0 10 10 :reverse-gaussian)
    (list 0
	  1.0616442880866572
	  3.61691635372837
	  6.35817808366385
	  8.339946278945918
	  9.395437997636988
	  9.824096362380532
	  9.959109345557367
	  9.992405685648272
	  9.998873144194922)
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
            {:cx 696.1450757976975,, :cy 522.1088068482732, :width 800, :height 600, :zoom 1}
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
            {:cx 740.3257005972225, :cy 555.244275447917, :width 800, :height 600, :zoom 1.7403626894942437}
	    {:cx 800, :cy 600, :width 800, :height 600, :zoom 2})
      ))))

;; (defn done []
;;   (prn "after  @c/camera" @c/camera)
;;   (is (= 1 1)))

;; (defn sleep [msec]
;;   (let [deadline (+ msec (.getTime (js/Date.)))]
;;     (while (> deadline (.getTime (js/Date.))))))

;; (deftest vec-svg-user->svg-px_basic
;;   (let [camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
;;         src-svg-user [400 300]
;;         dst-svg-user [500 300]]
;;     (is
;;      (=
;;       (#'c/vec-svg-user->svg-px camera src-svg-user dst-svg-user)
;;       [100 0]))))

;; (deftest vec-svg-user->svg-px_zoomed
;;   (let [camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 2}
;;         src-svg-user [400 300]
;;         dst-svg-user [500 300]]
;;     (is
;;      (=
;;       (#'c/vec-svg-user->svg-px camera src-svg-user dst-svg-user)
;;       [200 0]))))

;; (deftest move-camera_basic
;;   (let [initial-timestamp 0
;;         current-time 5000
;;         initial-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
;;         initial-mouse-pos-svg-px [500 300]
;;         dst-mouse-pos-svg-px [600 300]
;;         current-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
;;         ]

;;     (is
;;      (=
;;       (#'c/move-camera initial-timestamp current-time
;;                        initial-camera initial-mouse-pos-svg-px dst-mouse-pos-svg-px
;;                        current-camera)
;;       {:cx 500 :cy 300 :width 800 :height 600 :zoom 1}))
;;     ))

;; (deftest move-camera_already-moved
;;   (let [initial-timestamp 0
;;         current-time 5000
;;         initial-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 1}
;;         initial-mouse-pos-svg-px [500 300]
;;         dst-mouse-pos-svg-px [500 400]
;;         current-camera {:cx 500 :cy 300 :width 800 :height 600 :zoom 1}
;;         ]

;;     (is
;;      (=
;;       (#'c/move-camera initial-timestamp current-time
;;                        initial-camera initial-mouse-pos-svg-px dst-mouse-pos-svg-px
;;                        current-camera)
;;       {:cx 400 :cy 400 :width 800 :height 600 :zoom 1}))
;;     ))

;; (deftest move-camera_already-moved-zoomed
;;   (let [initial-timestamp 0
;;         current-time 5000
;;         initial-camera {:cx 400 :cy 300 :width 800 :height 600 :zoom 2}
;;         initial-mouse-pos-svg-px [500 300]
;;         dst-mouse-pos-svg-px [500 400]
;;         current-camera {:cx 600 :cy 300 :width 800 :height 600 :zoom 2}
;;         ]

;;     (is
;;      (=
;;       (#'c/move-camera initial-timestamp current-time
;;                        initial-camera initial-mouse-pos-svg-px dst-mouse-pos-svg-px
;;                        current-camera)
;;       {:cx 400 :cy 350 :width 800 :height 600 :zoom 2}))
;;     ))
