(ns bubble.core
  (:require [reagent.core :as reagent]
            [clojure.string :as string]
            [bubble.state :as state]
            [bubble.constant :as const]
            [bubble.event :as event]
            [bubble.drag :as drag]
            [bubble.coordinate :as coord]
            [bubble.build-link :as build-link]
            [cljs.core.async :refer [chan put! <! go-loop]]
            )
  )

(defn get-root-bubble []
  (state/get-bubble const/ROOT-BUBBLE-ID))

;;TODO: Instead of calling a state function directly,
;;      send a message to the event-queue
(defn toggle-bubble-validation [bubble-id]
  (let [validation-state (-> (state/get-bubble bubble-id) :done? not)]
    (state/update-bubble! bubble-id {:done? validation-state})))

;; (defn draw-pencil-button [edition?-atom visible? bubble-id cx cy rx ry]
(defn draw-pencil-button [visible? bubble-id cx cy rx ry]
  (let [semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset  (+ cx 25)
        y-offset  (- cy (+ ry max-bound 10))]
    [:g.button
     {:stroke "darkgreen"
      :stroke-width 2
      :transform (str "translate(" x-offset "," y-offset ")")
      :visibility (if visible? "visible" "hidden")
      :on-click
      (fn []
        ;; (reset! edition?-atom true)
        (put! event/event-queue [:enable-edition bubble-id]))
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

(defn draw-link-button [visible? bubble-id cx cy rx ry]
  (let [x-offset  (+ cx 60)
        y-offset  (- cy (+ ry 5))
        ]
    [:g.button
     {:stroke "darkblue"
      :stroke-width 4
      :transform (str "translate(" x-offset "," y-offset ") scale(1) rotate(-90)")
      :visibility (if visible? "visible" "hidden")
      :pointer-events "bounding-box"
      :on-click
      (build-link/build-link-start-fn bubble-id)
      }
     ;; Draw dash line
     (for [i (map #(* 2 %) (range 3))]
       (let [start (* 7 i)
             end (* 7 (inc i))]
         ^{:key (str i)} [:line {:x1 start :y1 start :x2 end :y2 end}]))
     ]))

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

(defn get-bubble-event-handling
  [bubble-id cx cy]
  (let [prevent-context-menu
        (fn [func]
          (fn [evt]
            (.preventDefault evt)
            (func)))]
    {
     :on-mouse-down
     (fn [evt]
       "
If the 'ctrl' is press during a click, build a link.
Else, drag the current bubble.
"
       (if (.-ctrlKey evt)
         (do
           ((build-link/build-link-start-fn bubble-id) evt)
           )
         (do
           ((drag/dragging-fn bubble-id) evt))
         ))

     :on-context-menu
     (prevent-context-menu
      #(put! event/event-queue [:delete-bubble bubble-id]))

     :on-click
     (build-link/build-link-end-fn bubble-id)
     }))

(defn bubble-text [initial-state? bubble-id]
  (let [dom-node (reagent/atom nil)
        {:keys [cy]} (state/get-bubble bubble-id)
        y-pos    (reagent/atom cy)]
    (reagent/create-class
     {
      :display-name "bubble-text"

      :component-did-mount
      (fn [this]
        (reset! dom-node (reagent/dom-node this))
        (update-bubble-size @dom-node bubble-id)
        (update-y-pos y-pos @dom-node bubble-id))

      :component-did-update
      (fn []
        (update-y-pos y-pos @dom-node bubble-id))

      :reagent-render
      (fn [initial-state bubble-id]
        (let [font-size 20
              text-style (if initial-state?
                           {:font-style "italic" :fill "#555"}
                           {:font-style "normal" :fill "#000"})
              {:keys [id cx cy]} (state/get-bubble bubble-id)
              ]
          [:text.label
           (merge (get-bubble-event-handling bubble-id cx cy)
                  {:style
                   (merge text-style
                          {
                           :text-anchor "middle"
                           :dominant-baseline "middle"
                           })
                   :y @y-pos
                   :font-size font-size
                   :on-double-click
                   #(put! event/event-queue [:enable-edition bubble-id])
                   })
           (let [counter (atom 0)
                 bubble (state/get-bubble bubble-id)
                 cx (:cx bubble)]
             (for [tspan-text (->> bubble :text string/split-lines)]
               (let [id-number @counter
                     tspan-id (str bubble-id @counter)]
                 (swap! counter inc)
                 ^{:key tspan-id} [:tspan {:x cx
                                           :dy (if (= id-number 0) 0 "1.2em")
                                           }
                                   tspan-text])))]))})))

(def ellipse-defaults
  {:stroke "black"
   :stroke-width 5
   })

(defn draw-ellipse-shape [bubble-id center-x center-y rx ry
                          new-center-x new-center-y done?
                          ]
  [:ellipse
   (merge ellipse-defaults
          (get-bubble-event-handling bubble-id center-x center-y)
          {:cx center-x
           :cy center-y
           :rx rx
           :ry ry
           :cursor "grab"
           :fill (if done? "#6f0" "#f06" )

           :on-double-click
           #(put! event/event-queue
                  [:create-bubble bubble-id new-center-x new-center-y])

           ;; :on-click
           ;; (build-link/build-link-end-fn bubble-id)

           })])

;;TODO: the root bubble must not be deletable
(defn draw-bubble-shape [bubble]
  (let [{:keys [id type cx cy rx ry done?]} bubble]
    (case type
      const/ROOT-BUBBLE-TYPE
      [:<>
       [draw-ellipse-shape
        id cx cy (+ 10 rx) (+ 10 ry)
        cx (- cy (* 3 ry)) done?]
       [draw-ellipse-shape
        id cx cy rx ry
        cx (- cy (* 3 ry)) done?]]

      const/BUBBLE-TYPE
      [draw-ellipse-shape
       id cx cy rx ry
       cx (- cy (* 3 ry)) done?]

      nil)))

(defn draw-delete-button [visible? bubble-id cx cy rx ry]
  (let [semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset  (- cx 25)
        y-offset  (- cy (+ ry max-bound 5))]
    [:g.button
     {:stroke "darkred"
      :stroke-width 5
      :transform (str "translate(" x-offset "," y-offset ")")
      :visibility (if visible? "visible" "hidden")
      :on-click
      #(put! event/event-queue [:delete-bubble bubble-id])
      }
     [:line {:x1 min-bound :y1 min-bound :x2 max-bound :y2 max-bound}]
     [:line {:x1 max-bound :y1 min-bound :x2 min-bound :y2 max-bound}]])
  )

(defn draw-validation-button [visible? bubble-id cx cy rx ry]
  (let [length 30
        x-offset  cx
        y-offset  (+ cy ry length 10)
        ]
    [:path.button
     {
      :stroke "darkgreen"
      :stroke-width 6
      :transform (str "translate(" x-offset "," y-offset ")")
      :visibility (if visible? "visible" "hidden")
      :pointer-events "bounding-box"
      :fill "none"
      :d (str "M " (- 0 (/ length 2)) "," (- 0 (/ length 2)) " L 0,0 L " length "," (- 0 length))
      :on-click #(toggle-bubble-validation bubble-id)
      }
     ])
  )

(defn add-button [bubble show-button?]
  (let [{:keys [id type cx cy rx ry text initial-state]} bubble
        initial-state? initial-state]
    (case type
      const/ROOT-BUBBLE-TYPE
      [:<>
       [draw-validation-button show-button? id cx cy (+ 10 rx) (+ 10 ry)]
       [draw-pencil-button show-button? id cx cy (+ 10 rx) (+ 10 ry)]
       [draw-link-button show-button? id cx cy (+ 10 rx) (+ 10 ry)]]

      const/BUBBLE-TYPE
      [:<>
       [draw-validation-button show-button? id cx cy rx ry]
       [draw-delete-button show-button? id cx cy rx ry]
       [draw-pencil-button show-button? id cx cy rx ry]
       [draw-link-button show-button? id cx cy rx ry]]

      nil)))

(defn draw-bubble [bubble]
  (let [show-button? (reagent/atom false)
        ]
    (fn [bubble]
      (let [{:keys [id initial-state? edition?]} bubble]

        ^{:key (str id "-group")}
        [:g
         {
          :on-mouse-over
          (fn []
            (if (state/get-link-src)
              (reset! show-button? false)
              (reset! show-button? true)))
          :on-mouse-leave #(reset! show-button? false)
          :pointer-events "bounding-box"
          }

         [draw-bubble-shape bubble]

         (when (not edition?)
           [add-button bubble @show-button?]
           )

         (if edition?
           [bubble-input bubble]
           [bubble-text initial-state? id]
           )
         ]))))

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
     :on-context-menu #(state/delete-link src-id dst-id)
     :stroke-width 4
     :stroke "black"
     :fill "none"
     :d path-str}
    )
  )

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
   [draw-bubble (get-root-bubble)]
   (doall
    (for [bubble (state/get-bubble-but-root)]
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
        :on-context-menu (fn [evt] (.preventDefault evt))
        }
       [all-bubble]
       ])}))
