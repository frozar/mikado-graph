(ns bubble.geometry
  (:require
   [bubble.constant :as const]))

(defn- float-euclidien-div
  "Realise a float division but keep a whole quotient, returns the remainder"
  [dividend divisor]
  (let [quot (-> (/ dividend divisor) js/Math.floor)
        remainder (- dividend (* divisor quot))]
    remainder))

(defn- get-relative-th0
  "Ensure the relative-th0 is in [-Pi , Pi["
  [th0 bubble-type]
  (let [relative-th0
        (->
         (case bubble-type
           :source th0
           :target (- th0 js/Math.PI))
         (+ js/Math.PI)
         (float-euclidien-div (* 2 js/Math.PI))
         (- js/Math.PI))]
    relative-th0))

(defn angle-between-bubbles
  "Compute the angle between the Ox axis and the vector [center-src-b center-dst-b]"
  ([{src-cx :cx src-cy :cy} {dst-cx :cx dst-cy :cy}]
   (angle-between-bubbles src-cx src-cy dst-cx dst-cy))
  ([src-cx src-cy dst-cx dst-cy]
   (let [[vector-x vector-y]
         [(- dst-cx src-cx) (- dst-cy src-cy)]]
     (js/Math.atan2 vector-y vector-x))))

(defn angle-between-bubble-position
  "Compute the angle between the Ox axis and the vector [center-src-b pt]"
  [src-b pt-x pt-y]
  (let [[vx vy]
        [(- pt-x (:cx src-b)) (- pt-y (:cy src-b))]]
    (js/Math.atan2 vy vx)))

(defn border-point
  "Given an incidental segment to the center of a bubble,
  compute the intersection point between the ellipse and the segment."
  ([{:keys [rx ry cx cy type]} th0 bubble-extremity]
   (border-point rx ry cx cy type th0 bubble-extremity))
  ([rx ry cx cy type th0 bubble-extremity]
   (let [relative-th0 (get-relative-th0 th0 bubble-extremity)

         [effective_rx effective_ry]
         (if (= type const/ROOT-BUBBLE-TYPE)
           [(+ rx const/ROOT-BUBBLE-OFFSET) (+ ry const/ROOT-BUBBLE-OFFSET)]
           [rx ry])

         t0 (js/Math.atan2 (* effective_rx (js/Math.tan relative-th0)) effective_ry)

         parametric_input
         (if (or (< (/ js/Math.PI 2) relative-th0)
                 (< relative-th0 (- 0 (/ js/Math.PI 2))))
           (+ t0 js/Math.PI)
           t0)
         ]
     ;; Use the ellipse parametric equation
     [(+ cx (* effective_rx (js/Math.cos parametric_input)))
      (+ cy (* effective_ry (js/Math.sin parametric_input)))])))

(defn incidental-border-points-between-bubbles
  "Return points on the border of bubbles which match with the intersection
  of the segment between the centers of the bubble and their border."
  ([{src-rx :rx src-ry :ry src-cx :cx src-cy :cy src-type :type}
    {dst-rx :rx dst-ry :ry dst-cx :cx dst-cy :cy dst-type :type}]
   (incidental-border-points-between-bubbles
    src-rx src-ry src-cx src-cy src-type
    dst-rx dst-ry dst-cx dst-cy dst-type))
  ([src-rx src-ry src-cx src-cy src-type
    dst-rx dst-ry dst-cx dst-cy dst-type]
   (let [th0 (angle-between-bubbles src-cx src-cy dst-cx dst-cy)
         [src-pt-x src-pt-y] (border-point src-rx src-ry src-cx src-cy src-type th0 :source)
         [dst-pt-x dst-pt-y] (border-point dst-rx dst-ry dst-cx dst-cy dst-type th0 :target)]
     [src-pt-x src-pt-y dst-pt-x dst-pt-y])))

(defn square-dist
  ([{x0 :x y0 :y} {x1 :x y1 :y}]
   (square-dist x0 y0 x1 y1))
  ([x0 y0 x1 y1]
   (let [square (fn [x] (* x x))]
     (+ (square (- x1 x0))
        (square (- y1 y0))))))

(defn dist [x0 y0 x1 y1]
  (let [square-dist (square-dist x0 y0 x1 y1)]
    (.sqrt js/Math square-dist)))

(defn radian->degree [theta]
  (-> theta
      (/ (.-PI js/Math))
      (* 180)))
