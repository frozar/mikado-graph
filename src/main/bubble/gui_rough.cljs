(ns bubble.gui-rough
  (:require
   [bubble.constant :as const]
   [bubble.event-factory :as event-factory]
   [bubble.gui-common :as gui-common]
   [bubble.state-read :as state-read]
   [clojure.string :as string]
   [reagent.core :as reagent]
   [reagent.dom :as rdom]
   [roughcljs.core :as rough]
   ))

(defn draw-building-link [bubble-src [mouse-x mouse-y]]
  (let [th0 (gui-common/angle-between-bubble-position bubble-src mouse-x mouse-y)
        [src-pt-x src-pt-y] (gui-common/border-point bubble-src th0 :source)
        ]
    (rough/line src-pt-x src-pt-y mouse-x mouse-y
                {:rough-option {:stroke "black"
                                :strokeWidth 2
                                :roughness 3
                                :roughnessGain 1
                                :seed 0}})))

(defn- link->path-str [src-b dst-b]
  (let [[src-pt-x src-pt-y dst-pt-x dst-pt-y]
        (gui-common/incidental-border-points-between-bubbles src-b dst-b)]
    (str "M " src-pt-x "," src-pt-y " L " dst-pt-x "," dst-pt-y)))

(defn- link->key-str [src-b dst-b]
  (let [src-id (:id src-b)
        dst-id (:id dst-b)]
    (str src-id "-" dst-id)))

(defn- draw-white-shadow-path
  [path-to-shallow]
  [:<>
   (-> path-to-shallow
       (update-in
        [2 1 :style]
        (fn [hashmap]
          (let [stroke-width-value (-> (:stroke-width hashmap) js/parseInt)]
            (assoc hashmap
                   :stroke "white"
                   :stroke-width (+ 3 stroke-width-value)))
          ))
       (update-in
        [1]
        (fn [hashmap]
          (let [key-value (:key hashmap)]
            (assoc hashmap
                   :key (str key-value "-shadow"))))))
   path-to-shallow])

(def rough-path-memoized (memoize rough/path))

(defn- draw-path
  ([src-b dst-b event-property] (draw-path src-b dst-b event-property true))
  ([src-b dst-b event-property to-shadow?]
   (let [path-str (link->path-str src-b dst-b)
         key-str (link->key-str src-b dst-b)
         rough-path (rough-path-memoized path-str
                                {:rough-option {:stroke "black"
                                                :strokeWidth 2
                                                :roughness 2
                                                :roughnessGain 1}
                                 :group-option (merge event-property
                                                      {:key key-str})})]
     (if to-shadow?
       [draw-white-shadow-path rough-path]
       rough-path))))

(defn- draw-arrowhead
  [src-b dst-b event-property]
  (let [th0 (gui-common/angle-between-bubbles src-b dst-b)
        [dst-pt-x dst-pt-y] (gui-common/border-point dst-b th0 :target)
        deg-th0 (/ (* th0 180) js/Math.PI)]
    (->
     (rough-path-memoized "M -10 -20 L 0 0 L 10 -20 L 0 -5 Z"
                          {:rough-option {:stroke "black"
                                          :strokeWidth 1}})
     (assoc 1 (merge event-property
                     {:transform
                      (str "translate(" dst-pt-x " " dst-pt-y ") "
                           "rotate("(+ deg-th0 -90)")")})))))

(defn- draw-link
  [src-b dst-b]
  (let [src-id (:id src-b)
        dst-id (:id dst-b)
        event-property (event-factory/event-property-factory :link src-id dst-id)]
    [:g
     {:class "arrow"}
     [draw-path src-b dst-b event-property]
     [draw-arrowhead src-b dst-b event-property]]))

(defn draw-links [couples_bubble]
  (when (seq couples_bubble)
    [:<>
     (doall
      (for [[src-b dst-b] couples_bubble]
        ^{:key (str (link->key-str src-b dst-b) "-link")}
        [:g
         {:class "graph_link"}
         [draw-link src-b dst-b]]))]))

(defn- draw-pencil-button
  [{:keys [cx cy]} ry show-button?
   event-properties]
  (let [semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset (+ cx 25)
        y-offset (- cy (+ ry max-bound 10))]
    [:g
     (merge event-properties
            {:class "button"
             :stroke "darkgreen"
             :stroke-width 2
             :transform (str "translate(" x-offset "," y-offset ")")
             :visibility (if show-button? "visible" "hidden")
             })
     ;; Draw a pencil
     ;; The body of the pencil
     [:line {:x1 max-bound :y1 min-bound :x2 min-bound :y2 max-bound}]
     [:line {:x1 max-bound :y1 min-bound :x2 min-bound :y2 max-bound
             :transform (str "translate(" 5 "," 5 ")")}]
     [:line {:x1 max-bound :y1 min-bound :x2 min-bound :y2 max-bound
             :transform (str "translate(" -5 "," -5 ")")}]
     [:line {:x1 (+ max-bound -5) :y1 (+ min-bound -5) :x2 (+ max-bound 5) :y2 (+ min-bound 5)}]
     ;; Pointer of the pencil
     [:line {:x1 (+ min-bound -5) :y1 (+ max-bound -5) :x2 (+ min-bound 5) :y2 (+ max-bound 5)}]
     [:line {:x1 (+ min-bound -5) :y1 (+ max-bound -5) :x2 (+ min-bound -5) :y2 (+ max-bound 5)}]
     [:line {:x1 (+ min-bound 5) :y1 (+ max-bound 5) :x2 (+ min-bound -5) :y2 (+ max-bound 5)}]
     ]
    )
  )

(defn- draw-link-button
  [{:keys [cx cy]} ry show-button?
   event-properties]
  (let [x-offset (+ cx 60)
        y-offset (- cy (+ ry 5))
        ]
    [:g
     (merge event-properties
            {:class "button"
             :stroke "darkblue"
             :stroke-width 4
             :transform (str "translate(" x-offset "," y-offset ") scale(1) rotate(-90)")
             :visibility (if show-button? "visible" "hidden")
             :pointer-events "bounding-box"
             })
     ;; Draw dash line
     (for [i (map #(* 2 %) (range 3))]
       (let [start (* 7 i)
             end (* 7 (inc i))]
         ^{:key (str i)} [:line {:x1 start :y1 start :x2 end :y2 end}]))
     ]))

(defn- draw-delete-button
  [{:keys [cx cy]} ry show-button?
   event-properties]
  (let [semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset  (- cx 25)
        y-offset  (- cy (+ ry max-bound 5))]
    [:g
     (merge event-properties
            {:class "button"
             :stroke "darkred"
             :stroke-width 5
             :transform (str "translate(" x-offset "," y-offset ")")
             :visibility (if show-button? "visible" "hidden")
             })
     [:line {:x1 min-bound :y1 min-bound :x2 max-bound :y2 max-bound}]
     [:line {:x1 max-bound :y1 min-bound :x2 min-bound :y2 max-bound}]])
  )

(defn- draw-validation-button
  [{:keys [cx cy]} ry show-button?
   event-properties]
  (let [length 30
        x-offset cx
        y-offset (+ cy ry length 10)
        ]
    [:path
     (merge event-properties
            {:class "button"
             :stroke "darkgreen"
             :stroke-width 6
             :transform (str "translate(" x-offset "," y-offset ")")
             :visibility (if show-button? "visible" "hidden")
             :fill "none"
             :d (str "M " (- 0 (/ length 2)) "," (- 0 (/ length 2)) " L 0,0 L " length "," (- 0 length))
             })
     ])
  )

(defn- add-button [{:keys [type ry] :as bubble} show-button?]
  (condp = type
    const/ROOT-BUBBLE-TYPE
    [:<>
     [draw-validation-button bubble (+ 10 ry) show-button?
      (event-factory/event-property-factory :validation-button bubble)]
     [draw-pencil-button bubble (+ 10 ry) show-button?
      (event-factory/event-property-factory :pencil-button bubble)]
     [draw-link-button bubble (+ 10 ry) show-button?
      (event-factory/event-property-factory :link-button bubble)]]

    const/BUBBLE-TYPE
    [:<>
     [draw-validation-button bubble ry show-button?
      (event-factory/event-property-factory :validation-button bubble)]
     [draw-delete-button bubble ry show-button?
      (event-factory/event-property-factory :delete-button bubble)]
     [draw-pencil-button bubble ry show-button?
      (event-factory/event-property-factory :pencil-button bubble)]
     [draw-link-button bubble ry show-button?
      (event-factory/event-property-factory :link-button bubble)]]

    nil))

(defn get-text-y [{:keys [cy text]} font-size]
  (let [nb-lines (-> text string/split-lines count)
        y-offset (-> nb-lines dec (* font-size) (/ 2))
        ]
    (- cy y-offset)
    ))

(defn- bubble-text-background-or-foreground
  [{:keys [id cx cy text] :as bubble} event-property font-size tspan-style]
  [:text
   (merge event-property
          {:class "label"
           :style {:text-anchor "middle"
                   :dominant-baseline "middle"}
           :y (get-text-y bubble font-size)})
   [:<>
    (for [[idx tspan-text]
          (map-indexed (fn [i text] [i text]) (string/split-lines text))]
      (let [tspan-id (str id cy idx "back")
            dy-value (if (= idx 0) 0 "1.2em")
            ]
        [:tspan
         (merge tspan-style
                {:key tspan-id
                 :x cx :dy dy-value})
         tspan-text]))]])

(defn- bubble-text
  [bubble event-property]
  (reagent/create-class
   {:display-name "bubble-text"

    :component-did-mount
    (fn [this]
      (gui-common/update-bubble-size (rdom/dom-node this) bubble))

    :reagent-render
    (fn [{:keys [initial-state?] :as bubble} event-property]
      (let [font-size (if initial-state? 20 24)

            tspan-style
            (if initial-state?
              {:font-size font-size :font-style "italic" :font-weight "bold" :fill "#555"}
              {:font-size font-size :font-style "normal" :fill "#000"})

            tspan-style-background
            (merge tspan-style {:filter "url(#bg-text)"})

            tspan-style-foreground
            tspan-style]
        [:<>
         (bubble-text-background-or-foreground
          bubble event-property font-size tspan-style-background)
         (bubble-text-background-or-foreground
          bubble event-property font-size tspan-style-foreground)
         ]
        ))}))

(def ellipse-memoized (memoize rough/ellipse))

(defn- draw-ellipse
  [{:keys [cx cy done? type]} rx ry event-property fill?]
  (->
   (ellipse-memoized
    0 0 (* 2 rx) (* 2 ry)
    {:rough-option
     {:seed 0
      :strokeWidth 3
      :fill (if fill?
              (if done? const/DONE-COLOR const/PENDING-COLOR)
              "none")
      :fillStyle "hachure"
      :fillWeight 1.5
      :hachureAngle 110
      :hachureGap
      (if (= type const/ROOT-BUBBLE-TYPE)
        10
        6)}})
   (assoc 1 (merge event-property {:transform (str "translate(" cx " " cy ")")}))))

(defn- draw-bubble [{:keys [id type rx ry edition?] :as bubble}]
  (let [show-button? (reagent/atom false)]
    (fn [{:keys [id type rx ry edition?] :as bubble}]
      (let [offset (if (type const/ROOT-BUBBLE-TYPE) const/ROOT-BUBBLE-OFFSET 0)
            event-property (event-factory/event-property-factory
                            :ellipse
                            bubble
                            (+ offset ry))]
        [:g
         {:class "bubble"
          :key (str id "-group")
          :pointer-events "bounding-box"
          :on-mouse-over
          (fn []
            (if (state-read/get-link-src)
              (reset! show-button? false)
              (reset! show-button? true)))
          :on-mouse-leave
          #(reset! show-button? false)}

         (condp = type
           const/ROOT-BUBBLE-TYPE
           [:<>
            [draw-ellipse bubble
             (+ const/ROOT-BUBBLE-OFFSET rx)
             (+ const/ROOT-BUBBLE-OFFSET ry) event-property true]
            [draw-ellipse bubble rx ry event-property false]]

           const/BUBBLE-TYPE
           [draw-ellipse bubble rx ry event-property true]

           nil)

         (if edition?
           [gui-common/bubble-input bubble]
           [:<>
            [bubble-text bubble (event-factory/event-property-factory :text bubble)]])]))))

(defn draw-bubbles [bubbles]
  [:<>
   (doall
    (for [[bubble-id bubble] bubbles]
      ^{:key bubble-id}
      [draw-bubble bubble]
      )
    )])
