(ns bubble.gui-common
  (:require
   [bubble.constant :as const]
   [bubble.event :as event]
   [cljs.core.async :refer [put!]]
   [clojure.string :as string]
   [reagent.core :as reagent]
   ))

(defn- center-textarea
  "Center the textarea field against the surrounding bubble"
  [dom-node {:keys [cx cy]}
   width-atom height-atom top-left-x-atom top-left-y-atom]
  (let [width (.-width (.getBoundingClientRect dom-node))
        height (.-height (.getBoundingClientRect dom-node))
        ]
    (reset! width-atom width)
    (reset! height-atom height)
    (reset! top-left-x-atom (- cx (/ width 2)))
    (reset! top-left-y-atom (- cy (/ height 2)))
    ))

(defn- cursor-to-end-textarea
  "Select the text in the textarea."
  [{:keys [initial-state?]}
   dom-node current-text]
  (let [text-length (count current-text)]
    (if initial-state?
      (do
        (set! (.-selectionStart dom-node) 0)
        (set! (.-selectionEnd dom-node) text-length))
      (.setSelectionRange dom-node text-length text-length)))
  )

(defn- get-nb-lines [s]
  (->> s (filter #(= % \newline)) count inc))

(defn- get-default-text [{:keys [type]}]
  (if (= type const/ROOT-BUBBLE-TYPE)
    const/ROOT-BUBBLE-DEFAULT-TEXT
    const/BUBBLE-DEFAULT-TEXT))

(defn- get-nb-columns [bubble current-text]
  (let [default-text
        (get-default-text bubble)

        default-text-length (count default-text)

        line-max-length
        (->> current-text
             string/split-lines
             (map count)
             (apply max))

        nb-columns
        (if (= line-max-length 0)
          default-text-length
          line-max-length)
        ]
    nb-columns
    )
  )

(defn update-bubble-size [dom-node {:keys [id]}]
  (let [width (.-width (.getBoundingClientRect dom-node))
        height (.-height (.getBoundingClientRect dom-node))
        radial-offset 40
        new-rx (-> width (/ 2) (+ radial-offset))
        new-ry (-> height (/ 2) (+ radial-offset))]
    (put! event/event-queue [:resize-bubble id new-rx new-ry])
    )
  )

(defn bubble-input
  "Create the input textarea tag to receive text updates."
  [{:keys [id rx ry cx cy text] :as bubble}]
  (let [stop
        #(put! event/event-queue [:disable-edition id])

        save
        (fn [text-inside-textarea]
          (let [input-text (-> text-inside-textarea str clojure.string/trim)]
            (if-not (empty? input-text)
              (put! event/event-queue [:save-text id input-text])
              )
            (stop)))

        current-text (reagent/atom text)
        dom-node (reagent/atom nil)

        width (reagent/atom (* 2 rx))
        height (reagent/atom (* 2 ry))
        top-left-x (reagent/atom cx)
        top-left-y (reagent/atom cy)
        ]
    (reagent/create-class
     {
      :display-name "bubble-input"

      :component-did-mount
      (fn [this]
        ;; Retrieve the textarea dom node from the foreignObject parent node
        (reset! dom-node (.-firstChild (reagent/dom-node this)))
        (.focus @dom-node)
        (center-textarea @dom-node bubble
                         width height top-left-x top-left-y)
        (update-bubble-size @dom-node bubble)
        (cursor-to-end-textarea bubble @dom-node @current-text)
        (reset! current-text text)
        )

      :component-did-update
      (fn []
        ;; Set the focus to the textarea
        (.focus @dom-node)
        (center-textarea @dom-node bubble
                         width height top-left-x top-left-y)
        (update-bubble-size @dom-node bubble)
        )

      :reagent-render
      (fn []
        (let [nb-lines (get-nb-lines @current-text)
              nb-columns (get-nb-columns bubble @current-text)]
          [:foreignObject
           {:style
            {:text-align "center"}
            :width @width
            :height @height
            :x @top-left-x
            :y @top-left-y
            }
           [:textarea
            {:style
             {:align "center"
              :overflow "hidden"
              :font-size "20px"
              :justify-content "center"
              :border "none"
              :-moz-text-align-last "center"
              :text-align-last "center"
              :resize "none"
              }
             :outline "none"
             :wrap "off"
             :placeholder (get-default-text bubble)
             :rows nb-lines
             :cols nb-columns
             :value @current-text

             :on-blur
             (fn [evt]
               (save (.. evt -target -value)))

             :on-change
             (fn [evt]
               (reset! current-text (.. evt -target -value))
               )

             :on-key-down
             (fn [evt]
               (let [enter-key-code 13
                     escape-key-code 27]
                 (condp = (.-keyCode evt)
                   enter-key-code (when (not (.-shiftKey evt))
                        (save (.. evt -target -value))
                        )
                   escape-key-code (stop)
                   nil)))

             }]]))})))

;; Helper functions
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

(defn- angle-between-bubbles
  "Compute the angle between the Ox axis and the vector [src-b dst-b]"
  [src-b dst-b]
  (let [[vx vy]
        [(- (:cx dst-b) (:cx src-b)) (- (:cy dst-b) (:cy src-b))]]
    (js/Math.atan2 vy vx)))

(defn- border-point
  "Given an incidental segment to the center of a bubble,
  compute the intersection point between the ellipse and the segment."
  [{:keys [rx ry cx cy]} th0 bubble-type]
  (let [relative-th0 (get-relative-th0 th0 bubble-type)

        t0 (js/Math.atan2 (* rx (js/Math.tan relative-th0)) ry)

        parametric_input
        (if (or (< (/ js/Math.PI 2) relative-th0)
                (< relative-th0 (- 0 (/ js/Math.PI 2))))
          (+ t0 js/Math.PI)
          t0)]
    ;; Use the ellipse parametric equation
    [(+ cx (* rx (js/Math.cos parametric_input)))
     (+ cy (* ry (js/Math.sin parametric_input)))]))

(defn incidental-border-points-between-bubbles
  "Return points on the border of bubbles which match with the intersection
  of the segment between the centers of the bubble and their border."
  [src-b dst-b]
  (let [th0 (angle-between-bubbles src-b dst-b)
        [src-pt-x src-pt-y] (border-point src-b th0 :source)
        [dst-pt-x dst-pt-y] (border-point dst-b th0 :target)]
    [src-pt-x src-pt-y dst-pt-x dst-pt-y]))

(comment
  (def src-b {:cx 0 :cy 0 :rx 2 :ry 1})
  (def dst-b {:cx 100 :cy 100 :rx 2 :ry 1})
  (def th0 (angle-between-bubbles src-b dst-b))
  (border-point src-b th0 :source)
  (for [i (range 8)]
    (border-point src-b (- (* i (/ (* 2 js/Math.PI) 8)) js/Math.PI) :source))
  (for [i (range 8)]
    (border-point dst-b (* i (/ (* 2 js/Math.PI) 8)) :target))
  (for [i (range 8)]
    (border-point dst-b (- (* i (/ (* 2 js/Math.PI) 8)) js/Math.PI) :target))
  )
