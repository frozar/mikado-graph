(ns svg-ellipse.bubble
  (:require [reagent.core :as reagent]
            [svg-ellipse.geometry :as g]
            [goog.events :as events]
            [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]
            [clojure.string :as string]
            ;; [cljs.test :refer-macros [deftest is testing run-tests]]
            )
  (:import [goog.events EventType]
           )
  )

;; (deftest test-numbers
;;   (is (= 1 1)))

(def ^:const NIL-BUBBLE-ID "not-initialized")
(def ^:const ROOT-BUBBLE-ID "root")

(def ^:const NIL-BUBBLE-TYPE  "nil-bubble")
(def ^:const ROOT-BUBBLE-TYPE "root-bubble")
(def ^:const BUBBLE-TYPE      "bubble")

(def ^:const BUBBLE-DEFAULT-TEXT      "New task")
(def ^:const ROOT-BUBBLE-DEFAULT-TEXT "Main goal")

(def nil-bubble
  {:id NIL-BUBBLE-ID
   :type NIL-BUBBLE-TYPE
   :initial-state? true
   :done? false
   :center (g/point 0 0)
   :rx 100 :ry 50
   :text BUBBLE-DEFAULT-TEXT
   })

(def root-bubble
  (merge nil-bubble
         {:id ROOT-BUBBLE-ID
          :type ROOT-BUBBLE-TYPE
          :initial-state? true
          :done? false
          :center (g/point 450 450)
          :rx 100
          :ry 50
          :text ROOT-BUBBLE-DEFAULT-TEXT
          }))

(defn initial-application-state []
  {
   :bubbles [root-bubble]
   :links []
   :link-src nil
   })

(defonce points
  (reagent/atom (initial-application-state)))

(defonce svg-bounding-box (reagent/atom nil))

;; Read/Write application state
(defn set-link-src [id]
  (swap! points update :link-src (fn [] id))
  )

(defn get-link-src []
  (:link-src @points)
  )

(defn reset-link-src []
  (swap! points update :link-src (fn [] nil)))

(defn get-bubble [id]
  (first (filter #(= (:id %) id) (:bubbles @points))))

(defn get-all-bubble []
  (:bubbles @points))

(defn get-root-bubble []
  (get-bubble ROOT-BUBBLE-ID))

(defn get-bubble-but-root []
  (filter #(not= (:id %) ROOT-BUBBLE-ID) (:bubbles @points)))

(defn add-bubble [bubble]
  (swap! points update :bubbles conj bubble))

(defn delete-bubble-shape [bubble-id]
  (swap! points update :bubbles (fn [l] (filterv #(not (= (:id %) bubble-id)) l))))

(defn add-link [id-src id-dst]
  (swap! points update :links conj {:src id-src :dst id-dst}))

(defn delete-link-to-bubble [bubble-id]
  (swap! points update :links (fn [l] (filterv
                                       (fn [link] not (= (some #{bubble-id} (vals link)) nil)) l))))

(defn delete-link [src-id dst-id]
  (swap! points update :links (fn [links]
                                (filterv
                                 (fn [link] (not= {:src src-id :dst dst-id} link))
                                 links))))

(defn update-link [bubble-id]
  (let [ids-dst (->> (:links @points)
                     (filterv (fn [link] (= bubble-id (:src link))))
                     (map :dst))
        ids-src (->> (:links @points)
                     (filterv (fn [link] (= bubble-id (:dst link))))
                     (map :src))
        new-links (vec (for [id-src ids-src
                             id-dst ids-dst]
                         {:src id-src :dst id-dst}))
        ]
    (delete-link-to-bubble bubble-id)
    (swap! points update :links (fn [l] (->> (apply conj l new-links)
                                             set
                                             vec)))
    ))

(defn update-bubble [bubble-id dictionary]
  (let [bubble (-> (get-bubble bubble-id) (merge dictionary))]
    (delete-bubble-shape bubble-id)
    (add-bubble bubble)))

(defn toggle-bubble-validation [bubble-id]
  (let [validation-state (-> (get-bubble bubble-id) :done? not)]
    (update-bubble bubble-id {:done? validation-state})))

;;
(defn gen-id
  "Generate a string of length 8, e.g.: 'b56d74c5'
  This generated id is not already present in the current application state"
  ([]
   (let [list-id (->> (get-all-bubble) (map :id))]
     (gen-id list-id)))
  ([list-id]
   (let [try-id (apply str (repeatedly 8 #(rand-nth "0123456789abcdef")))]
     (if (some #{try-id} list-id)
       (recur list-id)
       try-id))))

(defn get-svg-coord
  [bounding-client-rect x y]
  {:x (- x (.-left bounding-client-rect)) :y (- y (.-top bounding-client-rect))}
  )

(defn resize-bubble [bubble-id rx ry]
  (swap! points update :bubbles
         (fn [list-bubble]
           (let [list-idxs (map #(:id %) list-bubble)]
             ;; Check if the id to resize is present in the model.
             (if (some #{bubble-id} list-idxs)
               (update
                list-bubble
                (.indexOf list-idxs bubble-id)
                (fn [b] (merge b {:rx rx :ry ry}))
                ;; Else body
                list-bubble))))))


(defn move-bubble [id]
  (fn [x y]
    (swap!
     points update :bubbles
     (fn [list-bubble]
       (let [list-idxs (map #(:id %) list-bubble)]
         ;; Check if the id to delete is present in the model.
         ;; When the user drag a bubble on a right click, the move action
         ;; associated try to delete an id which was ready deleted.
         (if (some #{id} list-idxs)
           (update
            list-bubble
            (.indexOf list-idxs id)
            (fn [b] (merge b {:center (g/point x y)})))
           ;; Else body
           list-bubble))))))

(def ellipse-defaults
  {:style {;; /* webkit (safari, chrome) browsers */
           :-webkit-user-select "none"
           ;; /* mozilla browsers */
           :-moz-user-select "none"
           ;; /* webkit (konqueror) browsers */
           :-khtml-user-select "none"
           ;; /* IE10+ */
           :-ms-user-select "none"
           }
   :stroke "black"
   :stroke-width 5
   })

(defn drag-move-fn [on-drag cx cy]
  (let [first-evt-coord (atom nil)]
    (fn [evt]
      (let [{:keys [x y]} (get-svg-coord @svg-bounding-box (.-clientX evt) (.-clientY evt))
            current-x-evt x
            current-y-evt y]
        (if (nil? @first-evt-coord)
          (reset! first-evt-coord {:x current-x-evt :y current-y-evt}))
        (on-drag (+ cx (- current-x-evt (:x @first-evt-coord)))
                 (+ cy (- current-y-evt (:y @first-evt-coord))))))))

(defn drag-end-fn [drag-move drag-end-atom on-end]
  (fn [evt]
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (events/unlisten js/window EventType.MOUSEUP @drag-end-atom)
    (on-end)))

(defn dragging
  ([on-drag cx cy] (dragging on-drag (fn []) (fn []) cx cy))
  ([on-drag on-start on-end cx cy]
   (let [drag-move (drag-move-fn on-drag cx cy)
         drag-end-atom (atom nil)
         drag-end (drag-end-fn drag-move drag-end-atom on-end)]
     (on-start)
     (reset! drag-end-atom drag-end)
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))

(defn if-left-click [evt]
  (= 0 (.-button evt)))

(defn dragging-fn
  ([on-drag center]
   (fn [evt]
     (if (if-left-click evt)
       (dragging on-drag (g/x center) (g/y center)))))
  ([on-drag center-x center-y]
   (fn [evt]
     (if (if-left-click evt)
       (dragging on-drag center-x center-y))))
  )

(defn new-bubble [parent-bubble-id cx cy]
  (let [bubble-id (gen-id)
        new-bubble
        (merge nil-bubble {:id bubble-id :type BUBBLE-TYPE :center (g/point cx cy)})]
    (add-bubble new-bubble)
    (add-link parent-bubble-id bubble-id)
    ))

(defn delete-bubble [bubble-id]
  (fn [evt]
    (.preventDefault evt)
    (delete-bubble-shape bubble-id)
    (update-link bubble-id)
    (reset-link-src)
    ))

(defn draw-pencil-button [edition?-atom visible? bubble-id center rx ry]
  (let [semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset  (-> center g/x (+ 25))
        y-offset  (- (g/y center) (+ ry max-bound 10))]
    [:g.button
     {:stroke "darkgreen"
      :stroke-width 2
      :transform (str "translate(" x-offset "," y-offset ")")
      :visibility (if visible? "visible" "hidden")
      :on-click #(reset! edition?-atom true)
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

(defn draw-link-button [visible? bubble-id center rx ry]
  (let [x-offset  (-> center g/x (+ 60))
        y-offset  (- (g/y center) (+ ry 5))
        ]
    [:g.button
     {:stroke "darkblue"
      :stroke-width 4
      :transform (str "translate(" x-offset "," y-offset ") scale(1) rotate(-90)")
      :visibility (if visible? "visible" "hidden")
      :pointer-events "bounding-box"
      :on-click #(set-link-src bubble-id)
      }
     ;; Draw dash line
     (for [i (map #(* 2 %) (range 3))]
       (let [start (* 7 i)
             end (* 7 (inc i))]
         ^{:key (str i)} [:line {:x1 start :y1 start :x2 end :y2 end}]))
     ]))

(defn save-text-bubble [id text default-text]
  (swap!
   points update :bubbles
   (fn [list-bubble]
     (let [list-idxs (map #(:id %) list-bubble)]
       ;; Check if the id to delete is present in the model.
       ;; When the user drag a bubble on a right click, the move action
       ;; associated try to delete an id which was ready deleted.
       (if (some #{id} list-idxs)
         (do
           (let [current-bubble (get-bubble id)
                 initial-state-value (and (= text default-text)
                                          (:initial-state? current-bubble))]
             (update
              list-bubble
              (.indexOf list-idxs id)
              #(merge % {:text text :initial-state? initial-state-value}))))
         ;; Else body
         list-bubble)))))

(defn center-textarea [dom-node id center
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
    (reset! top-left-x-atom (- (g/x center) (/ width 2)))
    (reset! top-left-y-atom (- (g/y center) (/ height 2)))
    (resize-bubble id (add-50 (/ width 2)) (add-50 (/ height 2)))
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

(defn custom-textarea [bubble on-save on-stop
                       width-atom height-atom top-left-x-atom top-left-y-atom]
  (let [{:keys [id center text type initial-state?]} bubble
        current-text (reagent/atom text)
        dom-node (reagent/atom nil)
        stop #(if on-stop (on-stop))
        save (fn []
               (let [v (-> @current-text str clojure.string/trim)]
                 (if-not (empty? v)
                   (on-save v))
                 (stop)))]
    (reagent/create-class
     {
      :display-name "custom-textarea"

      :component-did-mount
      (fn [this]
        (reset! dom-node (reagent/dom-node this))
        ;; Set the focus to the textarea
        (.focus @dom-node)
        (center-textarea @dom-node id center
                         width-atom height-atom top-left-x-atom top-left-y-atom)
        (cursor-to-end-textarea @dom-node @current-text initial-state?)
        (reset! current-text text)
        )

      :component-did-update
      (fn []
        ;; Set the focus to the textarea
        (.focus @dom-node)
        (center-textarea @dom-node id center
                         width-atom height-atom top-left-x-atom top-left-y-atom))

      :reagent-render
      (fn [bubble on-save on-stop
           width-atom height-atom top-left-x-atom top-left-y-atom]
        (let [nb-lines (get-nb-lines @current-text)
              default-text (if (= type ROOT-BUBBLE-TYPE) ROOT-BUBBLE-DEFAULT-TEXT BUBBLE-DEFAULT-TEXT)
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

(defn bubble-input [bubble on-save on-stop]
  (let [{:keys [center rx ry]} bubble
        width (reagent/atom (* 2 rx))
        height (reagent/atom (* 2 ry))
        top-left-x (reagent/atom (g/x center))
        top-left-y (reagent/atom (g/y center))
        ]
    (fn [bubble on-save on-stop]
      [:foreignObject
       {:width @width
        :height @height
        :x @top-left-x
        :y @top-left-y
        }
       [custom-textarea bubble on-save on-stop
        width height top-left-x top-left-y]
       ])))

(defn update-bubble-size [dom-node bubble-id]
  (let [width (.-width (.getBoundingClientRect dom-node))
        height (.-height (.getBoundingClientRect dom-node))
        new-rx (-> width (/ 2) (+ 50))
        new-ry (-> height (/ 2) (+ 50))]
    (resize-bubble bubble-id new-rx new-ry)
    )
  )

(defn update-y-pos [y-pos-atom dom-node bubble-id]
  (let [height (.-height (.getBoundingClientRect dom-node))
        bubble (get-bubble bubble-id)
        y-bubble (g/y (:center bubble))
        nb-lines (->> bubble :text string/split-lines count)
        height-line (/ height nb-lines)
        y-offset (-> nb-lines dec (* height-line) (/ 2))
        ]
    (reset! y-pos-atom (- y-bubble y-offset))
    ))

(defn building-link-end [id-dst]
  (let [id-src (get-link-src)]
    (add-link id-src id-dst)
    (reset-link-src)))

(defn get-bubble-event-handling
  ([bubble-id center]
   (let [on-drag (move-bubble bubble-id)]
     {:on-mouse-down (dragging-fn on-drag center)
      :on-context-menu (delete-bubble bubble-id)}))
  ([bubble-id center-x center-y]
   (let [on-drag (move-bubble bubble-id)]
     {:on-mouse-down (dragging-fn on-drag center-x center-y)
      :on-context-menu (delete-bubble bubble-id)}))
  )

(defn bubble-text [edition?-atom initial-state? bubble-id]
  (let [dom-node (reagent/atom nil)
        y-pos    (reagent/atom 0)]
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
      (fn [edition?-atom initial-state bubble-id]
        (let [font-size 20
              text-style (if initial-state?
                           {:font-style "italic" :fill "#555"}
                           {:font-style "normal" :fill "#000"})
              {:keys [id center]} (get-bubble bubble-id)
              ]
          [:text.label (merge (get-bubble-event-handling bubble-id center)
                        {:style
                         (merge text-style
                          {
                           :text-anchor "middle"
                           :dominant-baseline "middle"
                           })
                         :y @y-pos
                         :font-size font-size
                         :on-double-click #(reset! edition?-atom true)
                         :on-click
                         (fn [evt]
                           (when (get-link-src)
                             (building-link-end bubble-id)
                             ))
                         })
           (let [counter (atom 0)
                 bubble (get-bubble bubble-id)
                 c (:center bubble)]
             (for [tspan-text (->> bubble :text string/split-lines)]
               (let [id-number @counter
                     tspan-id (str bubble-id @counter)]
                 (swap! counter inc)
                 ^{:key tspan-id} [:tspan {:x (g/x c)
                                           :dy (if (= id-number 0) 0 "1.2em")
                                           }
                                   tspan-text])))]))})))

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
           :on-double-click #(new-bubble bubble-id new-center-x new-center-y)
           :on-click
           (fn []
             (when (get-link-src)
               (building-link-end bubble-id)
               ))
           })])

(defn draw-bubble-shape [bubble]
  (let [{:keys [id type center rx ry done?]} bubble]
    (case type
      ROOT-BUBBLE-TYPE
      [:<>
       [draw-ellipse-shape
        id (g/x center) (g/y center) (+ 10 rx) (+ 10 ry)
        (g/x center) (- (g/y center) (* 3 ry)) done?]
       [draw-ellipse-shape
        id (g/x center) (g/y center) rx ry
        (g/x center) (- (g/y center) (* 3 ry)) done?]]

      BUBBLE-TYPE
      [draw-ellipse-shape
       id (g/x center) (g/y center) rx ry
       (g/x center) (- (g/y center) (* 3 ry)) done?]

      nil)))

(defn draw-delete-button [visible? bubble-id center rx ry]
  (let [semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset  (-> center g/x (- 25))
        y-offset  (- (g/y center) (+ ry max-bound 5))]
    [:g.button
     {:stroke "darkred"
      :stroke-width 5
      :transform (str "translate(" x-offset "," y-offset ")")
      :visibility (if visible? "visible" "hidden")
      :on-click (delete-bubble bubble-id)
      }
     [:line {:x1 min-bound :y1 min-bound :x2 max-bound :y2 max-bound}]
     [:line {:x1 max-bound :y1 min-bound :x2 min-bound :y2 max-bound}]])
  )

(defn draw-validation-button [visible? bubble-id center rx ry]
  (let [length 30
        x-offset  (-> center g/x )
        y-offset  (-> center g/y (+ ry length 10))
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

(defn add-button [bubble edition?-atom show-button?]
  (let [{:keys [id type center rx ry text initial-state]} bubble
        initial-state? initial-state]
    (case type
      ROOT-BUBBLE-TYPE
      [:<>
       [draw-validation-button show-button? id center (+ 10 rx) (+ 10 ry)]
       [draw-pencil-button edition?-atom show-button? id center (+ 10 rx) (+ 10 ry)]
       [draw-link-button show-button? id center (+ 10 rx) (+ 10 ry)]]

      BUBBLE-TYPE
      [:<>
       [draw-validation-button show-button? id center rx ry]
       [draw-delete-button show-button? id center rx ry]
       [draw-pencil-button edition?-atom show-button? id center rx ry]
       [draw-link-button show-button? id center rx ry]]

      nil)))

(defn draw-bubble [bubble]
  (let [edition? (reagent/atom false)
        show-button? (reagent/atom false)
        ]
    (fn [bubble]
      (let [{:keys [id type center rx ry initial-state?]} bubble
            on-save (fn[bubble-text] (save-text-bubble id bubble-text BUBBLE-DEFAULT-TEXT))
            on-stop #(reset! edition? false)
            ]
        ;; Throw an exception if one try to draw a nil-bubble
        (if (= type NIL-BUBBLE-TYPE)
          (throw (js/Error. "Try to draw nil-bubble!"))
          )

        ^{:key (str id "-g")}
        [:g
         {
          :on-mouse-over
          (fn [] (if (get-link-src)
                   (reset! show-button? false)
                   (reset! show-button? true)))
          :on-mouse-leave #(reset! show-button? false)
          :pointer-events "bounding-box"
          }

         [draw-bubble-shape bubble]

         (when (not @edition?)
           [add-button bubble edition? @show-button?]
           )

         (if @edition?
           [bubble-input bubble on-save on-stop]
           [bubble-text edition? initial-state? id]
           )
         ]))))

(defn get-link-path [link]
  (let [{:keys [src dst]} link
        src-b (get-bubble src)
        dst-b (get-bubble dst)
        src-id (:id src-b)
        dst-id (:id dst-b)
        src-pt (:center src-b)
        dst-pt (:center dst-b)
        path-str (str "M " (g/x src-pt) "," (g/y src-pt) " L " (g/x dst-pt) "," (g/y dst-pt))]
    {:key (str src-id "-" dst-id)
     :on-context-menu #(delete-link src-id dst-id)
     :stroke-width 4
     :stroke "black"
     :fill "none"
     :d path-str}
    )
  )

(defn draw-links []
  (let [links-path (doall (map (fn [link] (get-link-path link)) (:links @points)))]
    (when links-path
      [:g.graph_link
       (for [path links-path]
         [:<>
          [:path (merge path {:stroke-width 20 :stroke "white"})]
          [:path path]]
         )
       ])
    )
  )

(defn draw-building-link [mouse-svg-pos]
  (let [bubble-src-id (get-link-src)
        bubble-src (get-bubble bubble-src-id)
        {:keys [center]} bubble-src]
    [:line {:stroke "black"
            :stroke-width 5
            :x1 (g/x center) :y1 (g/y center)
            :x2 (:x mouse-svg-pos) :y2 (:y mouse-svg-pos)}]))

(defn all-bubble [mouse-svg-pos]
  [:g
   (draw-links)
   (when (get-link-src)
     [draw-building-link mouse-svg-pos])
   [draw-bubble (get-root-bubble)]
   (doall
    (for [bubble (get-bubble-but-root)]
      ^{:key (:id bubble)} [draw-bubble bubble]
      )
    )
   ])

(defn svg-canvas []
  (let [dom-node (reagent/atom nil)
        mouse-svg-pos (reagent/atom nil)
        ]
    (reagent/create-class
     {
      :display-name "svg-canvas"

      :component-did-mount
      (fn [this]
        (reset! dom-node (reagent/dom-node this))
        (reset! svg-bounding-box (.getBoundingClientRect @dom-node)))

      :reagent-render
      (fn []
        [:svg {:style {:border "1px solid"
                       :background "white"
                       ;; :width "800"
                       ;; :height "800"
                       }
               :on-context-menu (fn [evt] (.preventDefault evt))

               ;; :pointer-events "none"
               ;; :on-click (fn []
               ;;             (clog (:link-src @points))
               ;;             (reset-link-src)
               ;;             (clog (:link-src @points))
               ;;             )

               ;; :on-key-down (fn [evt]
               ;;                (clog (.-which evt))
               ;;                ;; 27: escape-keycode
               ;;                (case (.-which evt)
               ;;                  27 (reset-link-src)
               ;;                  nil))

               :on-mouse-move
               (fn [evt]
                 (reset! mouse-svg-pos (get-svg-coord
                                        @svg-bounding-box
                                        (.-clientX evt)
                                        (.-clientY evt)))
                 )
               }
         [all-bubble @mouse-svg-pos]
         ])})))
