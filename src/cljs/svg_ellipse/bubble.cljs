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

(def ROOT-ID "root")
(def BUBBLE-DEFAULT-TEXT "New task")
(def ROOT-BUBBLE-DEFAULT-TEXT "Main goal")

(def initial-application-state
  {
   :bubbles [{:id ROOT-ID
              :type :root-bubble
              :center (g/point 250 450)
              :rx 100
              :ry 50
              :text "Main goal"
              :initial-state true
              }]
   :links []
   :link-src nil
   })

(defonce points
  (reagent/atom initial-application-state))

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
  (clog (:links @points))
  (clog {:src src-id :dst dst-id})
  (clog (filterv (fn [link] (not= {:src src-id :dst dst-id} link)) (:links @points)))
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

;;
(defn gen-id []
  "Generate a string of length 8, e.g.: 'b56d74c5'"
  (apply str (repeatedly 8 #(rand-nth "0123456789abcdef"))))

(defn get-bcr [svg-root]
  (-> svg-root
      reagent/dom-node
      .getBoundingClientRect))

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
   :fill "#f06"
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

(defn dragging-fn [on-drag bubble]
  (fn [evt]
    (if (if-left-click evt)
      (dragging on-drag (g/x (:center bubble)) (g/y (:center bubble))))))

(defn bubble-init [bubble-id cx cy]
  {:id bubble-id :center (g/point cx cy)
   :type :bubble
   :rx 100 :ry 50
   :text BUBBLE-DEFAULT-TEXT
   :initial-state true})

(defn new-bubble [parent-bubble-id cx cy]
  (let [bubble-id (gen-id)
        new-bubble (bubble-init bubble-id cx cy)]
    ;; (swap! points update :bubbles conj (bubble-init bubble-id cx cy))
    (add-bubble new-bubble)
    ;; (swap! points update :links conj {:src parent-bubble-id :dst bubble-id})
    (add-link parent-bubble-id bubble-id)
    ))

(defn delete-bubble [bubble-id]
  (fn [evt]
    (.preventDefault evt)
    ;; (swap! points update :bubbles (fn [l] (filterv #(not (= (:id %) id)) l)))
    (delete-bubble-shape bubble-id)
    ;; (swap! points update :links (fn [l] (filterv
    ;;                                      (fn [link] not (= (some #{bubble-id} (vals link)) nil)) l)))
    ;; (clog (:links @points))

    ;; (let [ids-dst (clog (->> (:links @points)
    ;;                          (filterv (fn [link] (= bubble-id (:src link))))
    ;;                          (map :dst)))
    ;;       ids-src (clog (->> (:links @points)
    ;;                          (filterv (fn [link] (= bubble-id (:dst link))))
    ;;                          (map :src)))
    ;;       ]
    ;;   (clog ids-src)
    ;;   (clog ids-dst)
    ;;   (clog (vec (for [id-src ids-src
    ;;                    id-dst ids-dst]
    ;;                {:src id-src :dst id-dst})))
    ;;   )

    (update-link bubble-id)

    (reset-link-src)
    ))

(defn draw-pencil-button [edition?-atom visible? bubble-id center rx ry]
  (let [semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset  (-> center g/x (+ 25))
        y-offset  (- (g/y center) (+ ry max-bound 10))]
    [:g
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
  (let [semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset  (-> center g/x (+ 60))
        y-offset  (- (g/y center) (+ ry 5))
        ]
    [:g
     {:stroke "darkblue"
      :stroke-width 0.5
      :transform (str "translate(" x-offset "," y-offset ") scale(7) rotate(-90)")
      :visibility (if visible? "visible" "hidden")
      :pointer-events "bounding-box"
      :on-click #(set-link-src bubble-id)
      }
     ;; Draw dash line
     (for [i (map #(* 2 %) (range 3))]
       ^{:key (str i)} [:line {:x1 i :y1 i :x2 (inc i) :y2 (inc i)}])
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
                                          (:initial-state current-bubble))]
             (update
              list-bubble
              (.indexOf list-idxs id)
              #(merge % {:text text :initial-state initial-state-value}))))
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

(defn custom-textarea [id center text on-save on-stop
                       width-atom height-atom top-left-x-atom top-left-y-atom
                       initial-state?]
  (let [current-text (reagent/atom text)
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
        (clog ":component-did-update")
        ;; Set the focus to the textarea
        (.focus @dom-node)
        (center-textarea @dom-node id center
                         width-atom height-atom top-left-x-atom top-left-y-atom))

      :reagent-render
      (fn [id center text on-save on-stop
           width-atom height-atom top-left-x-atom top-left-y-atom
           initial-state?]
        (let [nb-lines (get-nb-lines @current-text)
              line-max-length (->> @current-text string/split-lines (map count) (apply max))]
           [:textarea
            {:style
             {
              :overflow "hidden"
              ;; :position "absolute"
              :font-size "20px"
              :justify-content "center"
              :border "none"
              :-moz-text-align-last "center"
              :text-align-last "center"
              :resize "none"
              }
             :otline "none"
             :wrap "off"
             :placeholder "New task"
             :rows nb-lines
             :cols line-max-length
             :value @current-text
             :on-blur #(save)
             :on-change (fn [evt]
                          (reset! current-text (.. evt -target -value))
                          (clog @current-text))
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

(defn bubble-input [{:keys [id center text rx ry initial-state on-save on-stop]}]
  (let [width (reagent/atom (* 2 rx))
        height (reagent/atom (* 2 ry))
        top-left-x (reagent/atom (g/x center))
        top-left-y (reagent/atom (g/y center))
        ]
    (fn [{:keys [c text rx ry on-save on-stop]}]
      [:foreignObject
       {:width @width
        :height @height
        :x @top-left-x
        :y @top-left-y
        }
       [custom-textarea id center text on-save on-stop
        width height top-left-x top-left-y
        initial-state]
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

(defn bubble-text [edition?-atom initial-state? common-behavior bubble-id]
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
      (fn [edition?-atom initial-state common-behavior bubble-id]
        (let [font-size 20
              text-style (if initial-state?
                           {:font-style "italic" :fill "#555"}
                           {:font-style "normal" :fill "#000"})
              ]
          [:text (merge common-behavior
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

(defn draw-ellipse-shape [ellipse-defaults common-behavior
                          bubble-id center-x center-y rx ry
                          new-center-x new-center-y
                          ]
  [:ellipse
   (merge ellipse-defaults common-behavior
          {:on-double-click #(new-bubble bubble-id new-center-x new-center-y)
           :on-click
           (fn []
             (when (get-link-src)
               (building-link-end bubble-id)
               ))
           :cx center-x
           :cy center-y
           :rx rx
           :ry ry
           })])

(defn draw-bubble-shape [bubble]
  (let [{:keys [id type center rx ry]} bubble
        on-drag (move-bubble id)
        common-behavior {:on-mouse-down (dragging-fn on-drag bubble)
                         :on-context-menu (delete-bubble id)}
        ]
    (case type
      :root-bubble
      [:<>
       [draw-ellipse-shape ellipse-defaults common-behavior
        id (g/x center) (g/y center) (+ 10 rx) (+ 10 ry)
        (g/x center) (- (g/y center) (* 3 ry))]
       [draw-ellipse-shape ellipse-defaults common-behavior
        id (g/x center) (g/y center) rx ry
        (g/x center) (- (g/y center) (* 3 ry))]]

      :bubble
      [draw-ellipse-shape ellipse-defaults common-behavior
       id (g/x center) (g/y center) rx ry
       (g/x center) (- (g/y center) (* 3 ry))]


      nil
      )))

(defn draw-delete-button [visible? bubble-id center rx ry]
  (let [semi-length 15
        min-bound (- 0 semi-length)
        max-bound semi-length
        x-offset  (-> center g/x (- 25))
        y-offset  (- (g/y center) (+ ry max-bound 5))]
    [:g
     {:stroke "darkred"
      :stroke-width 5
      :transform (str "translate(" x-offset "," y-offset ")")
      :visibility (if visible? "visible" "hidden")
      :on-click (delete-bubble bubble-id)
      }
     [:line {:x1 min-bound :y1 min-bound :x2 max-bound :y2 max-bound}]
     [:line {:x1 max-bound :y1 min-bound :x2 min-bound :y2 max-bound}]])
  )

(defn add-button [bubble-type
                  edition?-atom show-button? initial-state? common-behavior
                  bubble-id center rx ry]
  (case bubble-type
    :root-bubble
    [:<>
     [draw-pencil-button edition?-atom show-button? bubble-id center (+ 10 rx) (+ 10 ry)]
     [draw-link-button show-button? bubble-id center (+ 10 rx) (+ 10 ry)]
     [bubble-text edition?-atom initial-state? common-behavior bubble-id]]

    :bubble
    [:<>
     [draw-delete-button show-button? bubble-id center rx ry]
     [draw-pencil-button edition?-atom show-button? bubble-id center rx ry]
     [draw-link-button show-button? bubble-id center rx ry]
     [bubble-text edition?-atom initial-state? common-behavior bubble-id]]

    nil))

(defn draw-bubble [bubble]
  (let [edition? (reagent/atom false)
        show-button? (reagent/atom false)
        ]
    (fn [bubble]
      (let [{:keys [id type center rx ry]} bubble
            on-drag (move-bubble id)
            common-behavior {:on-mouse-down (dragging-fn on-drag bubble)
                             :on-context-menu (delete-bubble id)}
            on-save (fn[text] (save-text-bubble id text BUBBLE-DEFAULT-TEXT))
            on-stop #(reset! edition? false)
            initial-state? (:initial-state bubble)
            ]
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

         (if @edition?
           [bubble-input (merge bubble {:on-save on-save :on-stop on-stop})]
           [add-button type
            edition? @show-button? initial-state? common-behavior
            id center rx ry]
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
      [:g
       (for [path links-path]
         [:path path])
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
   [draw-bubble (get-bubble ROOT-ID)]
   (doall
    (for [bubble (filter #(not= (:id %) ROOT-ID) (:bubbles @points))]
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
                       :width "800"
                       :height "800"}
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
