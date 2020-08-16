(ns bubble.gui.background
  (:require
   [bubble.camera :as camera]
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

(defn- horizontal-line [x-length]
  [:line {:x1 0 :y1 0 :x2 x-length :y2 0
          :stroke-width 2 :stroke "#bbb"}])

(def memoized-horizontal-line (memoize horizontal-line))

(defn- vertical-line [y-length]
  [:line {:x1 0 :y1 0 :x2 0 :y2 y-length
          :stroke-width 2 :stroke "#bbb"}])

(def memoized-vertical-line (memoize vertical-line))

(defn grid []
  (let [view-bbox (camera/camera->view-bbox)
        {x0 :left x1 :right y0 :top y1 :bottom} view-bbox
        x-length (- x1 x0)
        y-length (- y1 y0)
        base 100]
    [:<>
     (doall
      (for [fixed-x (intern-discret-range base [x0 x1])]
        ^{:key (str "vertical-" fixed-x)}
        [:g
         {:transform (str "translate(" fixed-x " " y0 ")")}
         [memoized-vertical-line y-length]]))

     (doall
      (for [fixed-y (intern-discret-range base [y0 y1])]
        ^{:key (str "horizontal-" fixed-y)}
        [:g
         {:transform (str "translate(" x0 " " fixed-y ")")}
         [memoized-horizontal-line x-length]]))]))
