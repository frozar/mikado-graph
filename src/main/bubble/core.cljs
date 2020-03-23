(ns bubble.core
  (:require [reagent.core :as reagent]
            [clojure.string :as string]
            [bubble.state :as state]
            [bubble.constant :as const]
            [bubble.event :as event]
            [bubble.drag :as drag]
            [bubble.coordinate :as coord]
            [bubble.build-link :as build-link]
            [cljs.core.async :refer [put!]]
            )
  )

(defn draw-pencil-button [bubble rx ry]
  (let [{:keys [id cx cy show-button?]} bubble
        semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset (+ cx 25)
        y-offset (- cy (+ ry max-bound 10))]
    [:g
     {:class "button"
      :stroke "darkgreen"
      :stroke-width 2
      :transform (str "translate(" x-offset "," y-offset ")")
      :visibility (if show-button? "visible" "hidden")
      :on-click
      (fn []
        (put! event/event-queue [:enable-edition id]))
      }
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

(defn draw-link-button [bubble rx ry]
  (let [{:keys [id cx cy show-button?]} bubble
        x-offset (+ cx 60)
        y-offset (- cy (+ ry 5))
        ]
    [:g
     {:class "button"
      :stroke "darkblue"
      :stroke-width 4
      :transform (str "translate(" x-offset "," y-offset ") scale(1) rotate(-90)")
      :visibility (if show-button? "visible" "hidden")
      :pointer-events "bounding-box"
      :on-click
      (build-link/build-link-start-fn id)
      }
     ;; Draw dash line
     (for [i (map #(* 2 %) (range 3))]
       (let [start (* 7 i)
             end (* 7 (inc i))]
         ^{:key (str i)} [:line {:x1 start :y1 start :x2 end :y2 end}]))
     ]))

(defn draw-delete-button [bubble rx ry]
  (let [{:keys [id cx cy show-button?]} bubble
        semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset  (- cx 25)
        y-offset  (- cy (+ ry max-bound 5))]
    [:g
     {:class "button"
      :stroke "darkred"
      :stroke-width 5
      :transform (str "translate(" x-offset "," y-offset ")")
      :visibility (if show-button? "visible" "hidden")
      :on-click
      #(put! event/event-queue [:delete-bubble id])
      }
     [:line {:x1 min-bound :y1 min-bound :x2 max-bound :y2 max-bound}]
     [:line {:x1 max-bound :y1 min-bound :x2 min-bound :y2 max-bound}]])
  )

(defn draw-validation-button [bubble rx ry]
  (let [{:keys [id cx cy show-button?]} bubble
        length 30
        x-offset cx
        y-offset (+ cy ry length 10)
        ]
    [:path
     {
      :class "button"
      :stroke "darkgreen"
      :stroke-width 6
      :transform (str "translate(" x-offset "," y-offset ")")
      :visibility (if show-button? "visible" "hidden")
      :pointer-events "bounding-box"
      :fill "none"
      :d (str "M " (- 0 (/ length 2)) "," (- 0 (/ length 2)) " L 0,0 L " length "," (- 0 length))
      :on-click
      #(put! event/event-queue [:toggle-done-status id])
      }
     ])
  )

(defn add-button [bubble]
  (let [{:keys [type rx ry]} bubble
        ]
    (case type
      const/ROOT-BUBBLE-TYPE
      [:<>
       [draw-validation-button bubble (+ 10 rx) (+ 10 ry)]
       [draw-pencil-button bubble (+ 10 rx) (+ 10 ry)]
       [draw-link-button bubble (+ 10 rx) (+ 10 ry)]]

      const/BUBBLE-TYPE
      [:<>
       [draw-validation-button bubble rx ry]
       [draw-delete-button bubble rx ry]
       [draw-pencil-button bubble rx ry]
       [draw-link-button bubble rx ry]]

      nil)))

(defn center-textarea [dom-node id cx cy
                       width-atom height-atom top-left-x-atom top-left-y-atom]
  "Center the textarea field against the surrounding bubble"
  (let [width (.-width (.getBoundingClientRect dom-node))
        height (.-height (.getBoundingClientRect dom-node))
        x-offset (/ width 2)
        y-offset (/ height 2)
        add-50 (fn [v] (+ 50 v))
        ]
    (reset! width-atom width)
    (reset! height-atom height)
    (reset! top-left-x-atom (- cx (/ width 2)))
    (reset! top-left-y-atom (- cy (/ height 2)))
    (state/resize-bubble! id (add-50 (/ width 2)) (add-50 (/ height 2)))
    ))

(defn cursor-to-end-textarea [dom-node current-text initial-state?]
  "Select the text in the textarea."
  (let [text-length (count current-text)]
    (if initial-state?
      (do
        (set! (.-selectionStart dom-node) 0)
        (set! (.-selectionEnd dom-node) text-length))
      (.setSelectionRange dom-node text-length text-length)))
  )

(defn get-nb-lines [s]
  (->> s (filter #(= % \newline)) count inc))

(defn custom-textarea [bubble
                       width-atom height-atom top-left-x-atom top-left-y-atom]
  (let [{:keys [id cx cy text type initial-state?]} bubble
        current-text (reagent/atom text)
        dom-node (reagent/atom nil)
        stop #(put! event/event-queue [:disable-edition id])
        save
        (fn []
          (let [input-text (-> @current-text str clojure.string/trim)]
            (if-not (empty? input-text)
              ;; (on-save text)
              (put! event/event-queue [:save-text id input-text])
              )
            (stop)))]
    (reagent/create-class
     {
      :display-name "custom-textarea"

      :component-did-mount
      (fn [this]
        (reset! dom-node (reagent/dom-node this))
        ;; Set the focus to the textarea
        (.focus @dom-node)
        (center-textarea @dom-node id cx cy
                         width-atom height-atom top-left-x-atom top-left-y-atom)
        (cursor-to-end-textarea @dom-node @current-text initial-state?)
        (reset! current-text text)
        )

      :component-did-update
      (fn []
        ;; Set the focus to the textarea
        (.focus @dom-node)
        (center-textarea @dom-node id cx cy
                         width-atom height-atom top-left-x-atom top-left-y-atom))

      :reagent-render
      (fn [bubble
           width-atom height-atom top-left-x-atom top-left-y-atom]
        (let [nb-lines (get-nb-lines @current-text)
              default-text (if (= type const/ROOT-BUBBLE-TYPE) const/ROOT-BUBBLE-DEFAULT-TEXT const/BUBBLE-DEFAULT-TEXT)
              default-text-length (count default-text)
              line-max-length-tmp (->> @current-text string/split-lines (map count) (apply max))
              line-max-length (if (= line-max-length-tmp 0) default-text-length line-max-length-tmp)]
           [:textarea
            {:style
             {
              :overflow "hidden"
              :font-size "20px"
              :justify-content "center"
              :border "none"
              :-moz-text-align-last "center"
              :text-align-last "center"
              :resize "none"
              }
             :otline "none"
             :wrap "off"
             :placeholder default-text
             :rows nb-lines
             :cols line-max-length
             :value @current-text
             :on-blur #(save)
             :on-change (fn [evt]
                          (reset! current-text (.. evt -target -value))
                          )
             :on-key-down (fn [evt]
                            ;; 13: enter-keycode
                            ;; 27: escape-keycode
                            (case (.-which evt)
                              13 (do
                                   (when (.-ctrlKey evt)
                                     (save)
                                     )
                                   )
                              27 (stop)
                              nil))
             }]))})))

(defn bubble-input [bubble]
  (let [{:keys [cx cy rx ry]} bubble
        width (reagent/atom (* 2 rx))
        height (reagent/atom (* 2 ry))
        top-left-x (reagent/atom cx)
        top-left-y (reagent/atom cy)
        ]
    (fn [bubble]
      [:foreignObject
       {:width @width
        :height @height
        :x @top-left-x
        :y @top-left-y
        }
       [custom-textarea bubble
        width height top-left-x top-left-y]
       ])))

(defn update-bubble-size [dom-node bubble-id]
  (let [width (.-width (.getBoundingClientRect dom-node))
        height (.-height (.getBoundingClientRect dom-node))
        new-rx (-> width (/ 2) (+ 50))
        new-ry (-> height (/ 2) (+ 50))]
    (state/resize-bubble! bubble-id new-rx new-ry)
    )
  )

(defn update-y-pos [y-pos-atom dom-node bubble-id]
  (let [height (.-height (.getBoundingClientRect dom-node))
        bubble (state/get-bubble bubble-id)
        y-bubble (:cy bubble)
        nb-lines (->> bubble :text string/split-lines count)
        height-line (/ height nb-lines)
        y-offset (-> nb-lines dec (* height-line) (/ 2))
        ]
    (reset! y-pos-atom (- y-bubble y-offset))
    ))

(defn prevent-context-menu
  ([] (prevent-context-menu (fn [])))
  ([func]
   (fn [evt]
     (.preventDefault evt)
     (func))))

(defn get-bubble-event-handling
  [bubble ry]
  (let [{:keys [id type cx cy]} bubble
        [new-cx new-cy] [cx (- cy (* 3 ry ))]]
    {
     :on-mouse-down
     (fn [evt]
       "If the 'ctrl' is press during a click, build a link.
       Else, drag the current bubble.
       "
       (if (.-ctrlKey evt)
         (do
           ((build-link/build-link-start-fn id) evt)
           )
         (do
           ((drag/dragging-fn id) evt))
         ))

     :on-context-menu
     (if (not= type const/ROOT-BUBBLE-TYPE)
       (prevent-context-menu
        #(put! event/event-queue [:delete-bubble id])))

     :on-click
     (build-link/build-link-end-fn id)

     :on-double-click
     #(put! event/event-queue
            [:create-bubble id new-cx new-cy])

     }))

(defn bubble-text [bubble]
  (let [dom-node (reagent/atom nil)
        {:keys [id cy]} bubble
        y-pos (reagent/atom cy)]
    (reagent/create-class
     {
      :display-name "bubble-text"

      :component-did-mount
      (fn [this]
        (reset! dom-node (reagent/dom-node this))
        (update-bubble-size @dom-node id)
        (update-y-pos y-pos @dom-node id))

      :component-did-update
      (fn []
        (update-y-pos y-pos @dom-node id))

      :reagent-render
      (fn [bubble]
        (let [font-size 20
              {:keys [id initial-state? cx cy]} bubble
              text-style (if initial-state?
                           {:font-style "italic" :fill "#555"}
                           {:font-style "normal" :fill "#000"})
              for-counter (atom 0)
              ]
          [:text.label
           ;; give a fake ry value to get-bubble-event-handling
           (merge (get-bubble-event-handling bubble 42)
                  {:style
                   (merge text-style
                          {
                           :text-anchor "middle"
                           :dominant-baseline "middle"
                           })
                   :y @y-pos
                   :font-size font-size
                   ;; overwrite the on double click event
                   :on-double-click
                   #(put! event/event-queue [:enable-edition id])
                   })
           (for [tspan-text (->> bubble :text string/split-lines)]
             (let [id-number @for-counter
                   tspan-id (str id @for-counter)]
               (swap! for-counter inc)
               ^{:key tspan-id}
               [:tspan {:x cx
                        :dy (if (= id-number 0) 0 "1.2em")
                        }
                tspan-text]))]))})))

(defn draw-ellipse [bubble rx ry]
  (let [{:keys [id cx cy done?]} bubble]
    [:ellipse
     (merge (get-bubble-event-handling bubble ry)
            {:stroke "black"
             :stroke-width 5
             :cx cx
             :cy cy
             :rx rx
             :ry ry
             :cursor "grab"
             :fill (if done? "#6f0" "#f06")
             })]))

(defn draw-bubble [bubble]
  (fn [bubble]
    (let [{:keys [id type rx ry initial-state? edition?]} bubble]

      ^{:key (str id "-group")}
      [:g
       {
        :on-mouse-over
        (fn []
          (if (state/get-link-src)
            (put! event/event-queue [:disable-show-button id])
            (put! event/event-queue [:enable-show-button id])
            ))
        :on-mouse-leave
        (fn []
          (put! event/event-queue [:disable-show-button id])
          )
        :pointer-events "bounding-box"
        }

       (case type
         const/ROOT-BUBBLE-TYPE
         [:<>
          [draw-ellipse bubble (+ 10 rx) (+ 10 ry)]
          [draw-ellipse bubble rx ry]]

         const/BUBBLE-TYPE
         [draw-ellipse bubble rx ry]

         nil)

       (if edition?
         [bubble-input bubble]
         [:<>
          [bubble-text bubble]
          [add-button bubble]]
         )
       ])))

(defn get-link-path [link]
  (let [{:keys [src dst]} link
        src-b (state/get-bubble src)
        dst-b (state/get-bubble dst)
        src-id (:id src-b)
        dst-id (:id dst-b)
        src-pt-x (:cx src-b)
        src-pt-y (:cy src-b)
        dst-pt-x (:cx dst-b)
        dst-pt-y (:cy dst-b)
        path-str (str "M " src-pt-x "," src-pt-y " L " dst-pt-x "," dst-pt-y)]
    {:key (str src-id "-" dst-id)
     :on-context-menu
     #(put! event/event-queue [:delete-link src-id dst-id])
     :stroke-width 4
     :stroke "black"
     :fill "none"
     :d path-str}
    )
  )

;;TODO: draw arrow as link, not straight line
(defn draw-links []
  (let [links-path (doall (map get-link-path (state/get-links)))]
    (when links-path
      [:g
       (for [path links-path]
         (let [key-wider (str (path :key) "-wider")]
           ^{:key key-wider}
           [:g.graph_link
            [:path (merge path {:key key-wider :stroke-width 20 :stroke "white"})]
            [:path path]]
           ))
       ])
    )
  )

(defn draw-building-link []
  (let [bubble-src-id (state/get-link-src)
        bubble-src (state/get-bubble bubble-src-id)
        {:keys [center cx cy]} bubble-src
        [mouse-x mouse-y] (state/get-mouse-position)
        ]
    [:line {:stroke "black"
            :stroke-width 5
            :x1 cx :y1 cy
            :x2 mouse-x :y2 mouse-y
            }]))

(defn all-bubble []
  [:g
   ;; Interactive part
   (when (state/get-link-src)
     [draw-building-link])

   ;; Static part
   (draw-links)
   (doall
    (for [bubble (state/get-bubbles)]
      ^{:key (:id bubble)} [draw-bubble bubble]
      )
    )
   ])

(defn svg-canvas []
  (reagent/create-class
   {
    :display-name "svg-canvas"

    :component-did-mount
    (let [dom-node (reagent/atom nil)]
      (fn [this]
        (reset! dom-node (reagent/dom-node this))
        (let [svg-bbox-client (.getBoundingClientRect @dom-node)
              svg-origin-x (.-left svg-bbox-client)
              svg-origin-y (.-top svg-bbox-client)]
          (coord/init-svg-origin! svg-origin-x svg-origin-y))))

    :reagent-render
    (fn []
      [:svg
       {:id "svg-canvas"
        :style
        {:border "none"
         :background "white"
         :position "fixed"
         :top 0
         :left 0
         :height "100%"
         :width "100%"
         }

        :on-context-menu
        (prevent-context-menu)
        }
       [all-bubble]
       ])}))
