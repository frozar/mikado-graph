(ns bubble.gui-solid
  (:require
   [bubble.constant :as const]
   [bubble.event-factory :as event-factory]
   [bubble.gui-common :as gui-common]
   [bubble.state-read :as state-read]
   [clojure.string :as string]
   [reagent.core :as reagent]
   [reagent.dom :as rdom]
   ))

(defn draw-building-link [bubble-src [mouse-x mouse-y]]
  (let [{:keys [cx cy]} bubble-src
        ]
    [:line {:stroke "black"
            :stroke-width 5
            :x1 cx :y1 cy
            :x2 mouse-x :y2 mouse-y
            }]))

(defn- link->path-str [src-b dst-b]
  (let [[src-pt-x src-pt-y dst-pt-x dst-pt-y]
        (gui-common/incidental-border-points-between-bubbles src-b dst-b)]
    (str "M " src-pt-x "," src-pt-y " L " dst-pt-x "," dst-pt-y)))

(defn- link->key-str [src-b dst-b]
  (let [src-id (:id src-b)
        dst-id (:id dst-b)]
    (str src-id "-" dst-id)))

(defn- draw-path
  [src-b dst-b event-property]
  (let [path-str (link->path-str src-b dst-b)
        key-str (link->key-str src-b dst-b)]
    [:path
     (merge
      event-property
      {:key key-str
       :stroke-width 4
       :stroke "black"
       :fill "none"
       :d path-str})]))

(defn- draw-white-shadow-path
  [src-b dst-b event-property]
  (let [hashmap (second (draw-path src-b dst-b event-property))]
    [:path
     (merge
      hashmap
      {:key (str (link->key-str src-b dst-b) "-wider")
       :stroke-width 20
       :stroke "white"})]))

(defn- draw-arrowhead
  [src-b dst-b event-property]
  (let [th0 (gui-common/angle-between-bubbles src-b dst-b)
        [dst-pt-x dst-pt-y] (gui-common/border-point dst-b th0 :target)
        deg-th0 (/ (* th0 180) js/Math.PI)
        path-str "M -10 -20 L 0 0 L 10 -20 L 0 -5 Z"
        ]
    [:path
     (merge
      event-property
      {:stroke "black"
       :stroke-width 2
       :fill "none"
       :d path-str
       :transform
       (str "translate(" dst-pt-x " " dst-pt-y ") "
            "rotate("(+ deg-th0 -90)")")})]))

(defn- draw-link
  [src-b dst-b]
  (let [src-id (:id src-b)
        dst-id (:id dst-b)
        event-property (event-factory/event-property-factory :link src-id dst-id)]
    [:g
     {:class "arrow"}
     [draw-white-shadow-path src-b dst-b event-property]
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
             :visibility (if show-button? "visible" "hidden")})
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

(defn- bubble-text
  [bubble
   event-property]
  (reagent/create-class
   {:display-name "bubble-text"

    :component-did-mount
    (fn [this]
      (gui-common/update-bubble-size (rdom/dom-node this) bubble))

    :reagent-render
    (fn [bubble event-property]
      (let [font-size 20
            {:keys [id initial-state? cx]} bubble
            text-style (if initial-state?
                         {:font-style "italic" :fill "#333"}
                         {:font-style "normal" :fill "#000"})]
        [:text
         (merge event-property
                {:class "label"
                 :style
                 (merge text-style
                        {
                         :text-anchor "middle"
                         :dominant-baseline "middle"
                         })
                 :y (get-text-y bubble font-size)
                 :font-size font-size
                 })
         (for [[idx tspan-text]
               (map-indexed
                (fn [idx text] [idx text])
                (-> bubble :text string/split-lines))]
           (let [tspan-id (str id idx)]
             ^{:key tspan-id}
             [:tspan
              {:x cx
               :dy (if (= idx 0) 0 "1.2em")
               }
              tspan-text]))]))}))

(defn- draw-ellipse
  [{:keys [cx cy done?]} rx ry
   event-property]
  [:ellipse
   (merge event-property
          {:stroke "black"
           :stroke-width 5
           :cx cx
           :cy cy
           :rx rx
           :ry ry
           :cursor "grab"
           :fill (if done? "#6f0" "#f06")
           })])

(defn- draw-bubble
  [{:keys [id type rx ry edition?] :as bubble}]
  (let [show-button? (reagent/atom false)]
    (fn [{:keys [id type rx ry edition?] :as bubble}]
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
          [draw-ellipse bubble (+ 10 rx) (+ 10 ry)
           (event-factory/event-property-factory :ellipse bubble (+ 10 ry))]
          [draw-ellipse bubble rx ry
           (event-factory/event-property-factory :ellipse bubble ry)]]

         const/BUBBLE-TYPE
         [draw-ellipse bubble rx ry
          (event-factory/event-property-factory :ellipse bubble ry)]

         nil)

       (if edition?
         [gui-common/bubble-input bubble]
         [:<>
          [bubble-text bubble
           (event-factory/event-property-factory :text bubble)]
          [add-button bubble @show-button?]]
         )
       ])))

(defn draw-bubbles [bubbles]
  [:<>
   (doall
    (for [[bubble-id bubble] bubbles]
      ^{:key bubble-id}
      [draw-bubble bubble]
      )
    )])
