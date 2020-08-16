(ns bubble.gui.background
  (:require
   [camera.core :as camera]
   ))

(defn- intern-discret-range [base [x0 x1]]
  (let [lower-bound
        (-> x0
            (/ base)
            (js/Math.ceil)
            (* base))
        upper-bound
        (-> x1
            (/ base)
            (js/Math.floor)
            (* base))]
    (range lower-bound (inc upper-bound) base)))

(defn- horizontal-line [camera-zoom x-length]
  (let [stroke-width-svg-px (/ 1 camera-zoom)]
    [:line {:x1 0 :y1 0 :x2 x-length :y2 0
            :stroke-width stroke-width-svg-px
            :stroke "#bbb"}]))

(defn- vertical-line [camera-zoom y-length]
  (let [stroke-width-svg-px (/ 1 camera-zoom)]
    [:line {:x1 0 :y1 0 :x2 0 :y2 y-length
            :stroke-width stroke-width-svg-px
            :stroke "#bbb"}]))

(defn- choice-base-svg-user
  ([current-base-svg-user target-base-svg-px]
   (choice-base-svg-user (camera/get-state) current-base-svg-user target-base-svg-px))
  ([camera current-base-svg-user target-base-svg-px]
   (let [current-base-svg-px (camera/dist-svg-user->dist-svg-px camera current-base-svg-user)]
     (cond
       (< target-base-svg-px current-base-svg-px)
       (recur camera (/ current-base-svg-user 2) target-base-svg-px)
       (< current-base-svg-px (/ target-base-svg-px 2))
       (recur camera (* current-base-svg-user 2) target-base-svg-px)
       :else current-base-svg-user))))

(defn grid []
  (let [view-bbox (camera/camera->view-bbox)
        {x0 :left x1 :right y0 :top y1 :bottom} view-bbox
        x-length (- x1 x0)
        y-length (- y1 y0)
        base-svg-user (choice-base-svg-user 100 200)
        camera-zoom (:zoom (camera/get-state))]
    [:g
     {:id "background-grid"}
     (doall
      (for [fixed-x (intern-discret-range base-svg-user [x0 x1])]
        ^{:key (str "vertical-" fixed-x)}
        [:g
         {:transform (str "translate(" fixed-x " " y0 ")")}
         [vertical-line camera-zoom y-length]]))

     (doall
      (for [fixed-y (intern-discret-range base-svg-user [y0 y1])]
        ^{:key (str "horizontal-" fixed-y)}
        [:g
         {:transform (str "translate(" x0 " " fixed-y ")")}
         [horizontal-line camera-zoom x-length]]))]))
