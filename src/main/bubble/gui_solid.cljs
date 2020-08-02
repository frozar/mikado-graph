(ns bubble.gui-solid
  (:require
   [bubble.constant :as const]
   [bubble.event-factory :as event-factory]
   [bubble.geometry :as geometry]
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
        (geometry/incidental-border-points-between-bubbles src-b dst-b)]
    (str "M " src-pt-x "," src-pt-y " L " dst-pt-x "," dst-pt-y)))

(defn- link->key-str [src-b dst-b]
  (let [src-id (:id src-b)
        dst-id (:id dst-b)]
    (str src-id "-" dst-id)))

(defn- draw-path
  [src-b dst-b event-property]
  (let [key-str (link->key-str src-b dst-b)
        [src-pt-x src-pt-y dst-pt-x dst-pt-y]
        (geometry/incidental-border-points-between-bubbles src-b dst-b)
        arrow-length (geometry/dist src-pt-x src-pt-y dst-pt-x dst-pt-y)
        path-str (str "M 0,0 L " arrow-length ",0")]
    ;; (js/console.log "arrow-length " arrow-length)
    ;; (js/console.log "path-str " path-str)
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
  (let [path-str "M -10 -20 L 0 0 L 10 -20 L 0 -5 Z"
        [src-pt-x src-pt-y dst-pt-x dst-pt-y]
        (geometry/incidental-border-points-between-bubbles src-b dst-b)
        arrow-length (geometry/dist src-pt-x src-pt-y dst-pt-x dst-pt-y)
        ]
    [:path
     (merge
      event-property
      {:stroke "black"
       :stroke-width 2
       :fill "none"
       :d path-str
       :transform
       (str "translate(" arrow-length " 0) "
            "rotate(" -90 ")")})]))

;; (defn draw-link
;;   [src-b dst-b]
;;   (let [src-id (:id src-b)
;;         dst-id (:id dst-b)
;;         event-property (event-factory/event-property-factory :link src-id dst-id)
;;         [src-pt-x src-pt-y _ _]
;;         (geometry/incidental-border-points-between-bubbles src-b dst-b)
;;         rad-th0 (geometry/angle-between-bubbles src-b dst-b)
;;         deg-th0 (geometry/radian->degree rad-th0)]
;;     ;; (js/console.log "1 draw-link (str src-id dst-id) " (str src-id "-" dst-id))
;;     [:g
;;      {:class "link"
;;       :id (str src-id "-" dst-id)
;;       :transform (str "translate(" src-pt-x " " src-pt-y ") "
;;                       "rotate(" deg-th0 ")")
;;       }
;;      [draw-white-shadow-path src-b dst-b event-property]
;;      [draw-path src-b dst-b event-property]
;;      [draw-arrowhead src-b dst-b event-property]]))

(defn- draw-link-render [src-b dst-b]
  (let [src-id (:id src-b)
        dst-id (:id dst-b)
        event-property (event-factory/event-property-factory :link src-id dst-id)
        [src-pt-x src-pt-y _ _]
        (geometry/incidental-border-points-between-bubbles src-b dst-b)
        rad-th0 (geometry/angle-between-bubbles src-b dst-b)
        deg-th0 (geometry/radian->degree rad-th0)]
    ;; (js/console.log "IN draw-link-render")
    [:g
     {:class "link"
      :id (str src-id "-" dst-id)
      :transform (str "translate(" src-pt-x " " src-pt-y ") "
                      "rotate(" deg-th0 ")")
      }
     [draw-white-shadow-path src-b dst-b event-property]
     [draw-path src-b dst-b event-property]
     [draw-arrowhead src-b dst-b event-property]]))

(defn draw-link
  [src-b dst-b]
  (reagent/create-class
   {:display-name "bubble-link"

    ;; :get-snapshot-before-update
    ;; (fn [this old-argv new-argv]
    ;;   (js/console.log ":get-snapshot-before-update")
    ;;   (js/console.log "old-argv " old-argv)
    ;;   (js/console.log "new-argv " new-argv)
    ;;   ;; (reagent/force-update this true)
    ;;   nil)

    ;; :should-component-update
    ;; (fn [this old-argv new-argv]
    ;;   ;; (js/console.log ":should-component-update")
    ;;   ;; (js/console.log "old-argv " old-argv)
    ;;   ;; (js/console.log "new-argv " new-argv)
    ;;   false)

    ;; :component-did-mount
    ;; (fn [this] (js/console.log ":component-did-mount"))

    ;; :component-will-unmount
    ;; (fn [this] (js/console.log ":component-will-unmount"))

    ;; :component-did-catch
    ;; (fn [this error info] (js/console.log ":component-did-catch"))

    ;; :component-did-update
    ;; (fn [this]
    ;;   ;; (gui-common/update-bubble-size (rdom/dom-node this) bubble)
    ;;   (js/console.log ":component-did-update")
    ;;   (js/console.log "this " this)
    ;;   ;; (reagent/flush)
    ;;   ;; (reagent/force-update this true)
    ;;   ;; (reagent/force-update this)
    ;;   ;; (draw-link-render src-b dst-b)
    ;;   )

    :reagent-render
    #_(draw-link-render src-b dst-b)
    (fn [src-b dst-b]
      ;; (js/console.log ":reagent-render")
      (draw-link-render src-b dst-b))
    #_(fn [src-b dst-b]
      (let [src-id (:id src-b)
            dst-id (:id dst-b)
            event-property (event-factory/event-property-factory :link src-id dst-id)
            [src-pt-x src-pt-y _ _]
            (geometry/incidental-border-points-between-bubbles src-b dst-b)
            rad-th0 (geometry/angle-between-bubbles src-b dst-b)
            deg-th0 (geometry/radian->degree rad-th0)]
        ;; (js/console.log "1 draw-link (str src-id dst-id) " (str src-id "-" dst-id))
        [:g
         {:class "link"
          :id (str src-id "-" dst-id)
          :transform (str "translate(" src-pt-x " " src-pt-y ") "
                          "rotate(" deg-th0 ")")
          }
         [draw-white-shadow-path src-b dst-b event-property]
         [draw-path src-b dst-b event-property]
         [draw-arrowhead src-b dst-b event-property]]))}))


(defn draw-links [couples_bubble]
  (when (seq couples_bubble)
    ;; (js/console.log "0 draw-links couples_bubble " couples_bubble)
    [:g
     {:id "links"}
     (doall
      (for [[src-b dst-b] couples_bubble]
        ^{:key (str (link->key-str src-b dst-b) "-link")}
        [draw-link src-b dst-b]))]))

(defn- draw-pencil-button
  [{:keys [cx cy]} ry show-button?
   event-properties]
  (let [semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset (+ 0 #_cx 25)
        y-offset (- 0 #_cy (+ ry max-bound 10))]
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
  (let [x-offset (+ 0 #_cx 60)
        y-offset (- 0 #_cy (+ ry 5))
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
        x-offset  (- 0 #_cx 25)
        y-offset  (- 0 #_cy (+ ry max-bound 5))]
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
        x-offset 0 ;; cx
        y-offset (+ 0 #_cy ry length 10)
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
    (- 0 y-offset)
    ))

(defn- bubble-text
  [bubble event-property]
  (reagent/create-class
   {:display-name "bubble-text"

    :component-did-mount
    (fn [this]
      (gui-common/update-bubble-size (rdom/dom-node this) bubble))

    :reagent-render
    (fn [{:keys [id initial-state? cx] :as bubble} event-property]
      (let [font-size (if initial-state? 20 24)

            text-style
            (if initial-state?
              {:font-size font-size :font-style "italic" :font-weight "bold" :fill "#555"}
              {:font-size font-size :font-style "normal" :fill "#000"})]
        [:text
         (merge event-property
                {:class "label"
                 :style
                 (merge text-style
                        {:text-anchor "middle"
                         :dominant-baseline "middle"})
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
              {:x 0 ;; cx
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
           :cx 0 ;; cx
           :cy 0 ;; cy
           :rx rx
           :ry ry
           :cursor "grab"
           :fill (if done? "#6f0" "#f06")
           })])

(defn- draw-bubble
  ;; [{:keys [id type rx ry edition?] :as bubble}]
  [bubble]
  (let [show-button? (reagent/atom false)]
    (fn [{:keys [id type cx cy rx ry edition?] :as bubble}]
      [:g
       {:class "bubble"
        :key (str id "-group")
        :transform (str "translate(" cx " " cy ")")
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
  [:g
   {:id "bubbles"}
   (doall
    (for [[bubble-id bubble] bubbles]
      ^{:key bubble-id}
      [draw-bubble bubble]
      )
    )])
