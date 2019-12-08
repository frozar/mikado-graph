(ns svg-ellipse.bubble
  (:require [reagent.core :as reagent]
            [svg-ellipse.geometry :as g]
            [goog.events :as events]
            [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]
            [clojure.string :as string]
            )
  (:import [goog.events EventType]
           )
  )

(def root-id "root")

(def initial-application-state
  {
   :bubbles [{:id root-id
              :center (g/point 250 450)
              :rx 100
              :ry 50
              :text "Main goal"
              :initial-state true
              }]
   :links []
   })

(defonce points
  (reagent/atom initial-application-state))

(defn get-bubble [id]
  (first (filter #(= (:id %) id) (:bubbles @points))))

(defn gen-id []
  "Generate a string of length 8, e.g.: 'b56d74c5'"
  (apply str (repeatedly 8 #(rand-nth "0123456789abcdef"))))

(defn get-bcr [svg-root]
  (-> svg-root
      reagent/dom-node
      .getBoundingClientRect))

(defn get-svg-coord [svg-root x y]
  (let [bcr (get-bcr svg-root)]
    {:x (- x (.-left bcr)) :y (- y (.-top bcr))}))

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


(defn move-bubble [svg-root id]
  ;; (clog id)
  ;; (clog (:bubbles @points))
  ;; (clog (map #(:id %) (:bubbles @points)))
  (fn [x y]
    (let [bcr (get-bcr svg-root)]
      ;; (js/console.log bcr)
      ;; (clog (.-left bcr))
      ;; (clog (.-right bcr))
      (swap!
       points update :bubbles
       (fn [list-bubble]
         (let [list-idxs (map #(:id %) list-bubble)]
           ;; (clog id)
           ;; (clog list-idxs)
           ;; (clog (.indexOf list-idxs id))
           ;; (clog (.-left bcr))
           ;; (clog (- x (.-left bcr)))
           ;; Check if the id to delete is present in the model.
           ;; When the user drag a bubble on a right click, the move action
           ;; associated try to delete an id which was ready deleted.
           (if (some #{id} list-idxs)
             (update
              list-bubble
              (.indexOf list-idxs id)
              ;; #(merge % {:center (g/point (- x (.-left bcr)) (- y (.-top bcr)))}))
              ;; #(merge % {:center (g/point x (- y (.-top bcr)))}))
              (fn [b] (merge b {:center (g/point x y)}))
              ;; #(merge % {:center (g/point x y)}))
             ;; Else body
             list-bubble))))))))

(def ellipse-defaults
  {
   :fill "#f06"
   :stroke "black"
   :stroke-width 5
   })

(defn drag-move-fn [on-drag cx cy svg-root]
  (let [first-evt-coord (atom nil)]
    (fn [evt]
      (let [{:keys [x y]} (get-svg-coord svg-root (.-clientX evt) (.-clientY evt))
            current-x-evt x
            current-y-evt y]
        (if (nil? @first-evt-coord)
          (reset! first-evt-coord {:x current-x-evt :y current-y-evt}))
        (on-drag (+ cx (- current-x-evt (:x @first-evt-coord)))
                 (+ cy (- current-y-evt (:y @first-evt-coord))))))))

(defn drag-end-fn [drag-move drag-end on-end]
  (fn [evt]
    (events/unlisten js/window EventType.MOUSEMOVE drag-move)
    (events/unlisten js/window EventType.MOUSEUP @drag-end)
    (on-end)))

(defn dragging
  ([on-drag cx cy svg-root] (dragging on-drag (fn []) (fn []) cx cy svg-root))
  ([on-drag on-start on-end cx cy svg-root]
   (let [drag-move (drag-move-fn on-drag cx cy svg-root)
         drag-end-atom (atom nil)
         drag-end (drag-end-fn drag-move drag-end-atom on-end)]
     (on-start)
     (reset! drag-end-atom drag-end)
     (events/listen js/window EventType.MOUSEMOVE drag-move)
     (events/listen js/window EventType.MOUSEUP drag-end))))

(defn if-left-click [evt]
  (= 0 (.-button evt)))

(defn dragging-fn [on-drag bubble svg-root]
  (fn [evt]
    (if (if-left-click evt)
      (dragging on-drag (g/x (:center bubble)) (g/y (:center bubble)) svg-root))))

(def NEW-TASK "New task")

(defn bubble-init [bubble-id cx cy]
  {:id bubble-id :center (g/point cx cy)
   :rx 100 :ry 50
   ;; :text (str "bubble " bubble-id)
   :text NEW-TASK
   :initial-state true})

(defn new-bubble [parent-bubble-id cx cy]
  (let [bubble-id (gen-id)]
    (swap! points update :bubbles conj (bubble-init bubble-id cx cy))
    ;; (clog parent-bubble-id)
    ;; (clog bubble-id)
    ;; (clog (get-bubble parent-bubble-id))
    ;; (clog (get-bubble bubble-id))
    (swap! points update :links conj {:src parent-bubble-id :dst bubble-id})
    ;; (clog (:links @points))
    ))

(defn delete-bubble [id]
  (fn [evt]
    (.preventDefault evt)
    (swap! points update :bubbles (fn [l] (filterv #(not (= (:id %) id)) l)))
    (swap! points update :links (fn [l] (filterv
                                         (fn [link] not (= (some #{id} (vals link)) nil)) l)))
    ))

(defn draw-root-bubble [svg-root]
  (let [root-bubble (get-bubble root-id)
        {:keys [center rx ry]} root-bubble
        basic-option {:cx (g/x center) :cy (g/y center)}
        on-drag (move-bubble svg-root root-id)]
    ;; [:<>
    [:g {
         :on-mouse-down (dragging-fn on-drag root-bubble svg-root)
         :on-double-click #(new-bubble (:id root-bubble) (g/x center) (- (g/y center) (* 3 ry)))
         :on-context-menu (fn [evt] (.preventDefault evt))
         }
     [:ellipse
      (merge ellipse-defaults basic-option
             {:rx (+ 10 rx) :ry (+ 10 ry)})]
     [:ellipse
      (merge ellipse-defaults basic-option
             {:rx rx :ry ry})]
     [:text {:style {:-webkit-user-select "none"
                     :-moz-user-select "none"
                     :text-anchor "middle"
                     :dominant-baseline "middle"}
             :x (g/x center) :y (g/y center) :font-size 20}
      (:text root-bubble)]
     ]
    )
  )

(defn save-text-bubble [id text]
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
                 initial-state-value (and (= text NEW-TASK) (:initial-state current-bubble))]
             (clog initial-state-value)
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
                                   (when (not (.-shiftKey evt))
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


(defn update-y-pos [y-pos-atom dom-node bubble-id]
  (let [height (.-height (.getBoundingClientRect dom-node))
        bubble (get-bubble bubble-id)
        y-bubble (g/y (:center bubble))
        nb-lines (->> bubble :text string/split-lines count)
        height-line (/ height nb-lines)
        y-offset (-> nb-lines dec (* height-line) (/ 2))
        ]
    (reset! y-pos-atom (- y-bubble y-offset))
    ;; (clog "update y-pos")
    ;; (clog @points)
    ))

(defn update-bubble-size [dom-node bubble-id]
  (let [width (.-width (.getBoundingClientRect dom-node))
        height (.-height (.getBoundingClientRect dom-node))
        new-rx (-> width (/ 2) (+ 50))
        new-ry (-> height (/ 2) (+ 50))]
    (resize-bubble bubble-id new-rx new-ry)
    )
  )

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
                         })
           (let [counter (atom 0)
                 bubble (get-bubble bubble-id)
                 c (:center bubble)]
             ;; (clog bubble)
             (for [tspan-text (->> bubble :text string/split-lines)]
               (let [id-number @counter
                     tspan-id (str bubble-id @counter)]
                 (swap! counter inc)
                 ^{:key tspan-id} [:tspan {:x (g/x c)
                                           :dy (if (= id-number 0) 0 "1.2em")
                                           }
                                   tspan-text])))]))})))

(defn draw-bubble [svg-root bubble]
  (let [edition? (reagent/atom false)
        ;; rx-atom  (reagent/atom (:rx bubble))
        ;; ry-atom  (reagent/atom (:ry bubble))
        ]
    (fn [svg-root bubble]
      (let [{:keys [id center rx ry]} bubble
            on-drag (move-bubble svg-root id)
            common-behavior {:on-mouse-down (dragging-fn on-drag bubble svg-root)
                             :on-context-menu (delete-bubble id)}
            on-save (fn[text] (save-text-bubble id text))
            on-stop #(reset! edition? false)
            initial-state? (:initial-state bubble)
            ]
        ^{:key (str id "-g")}
        [:g
         [:ellipse
          (merge ellipse-defaults common-behavior
                 {;; TODO stop the event propagation to avoid the
                  ;; text selection during drag
                  :on-double-click #(new-bubble id (g/x center) (- (g/y center) (* 3 ry)))
                  :cx (g/x center)
                  :cy (g/y center)
                  :rx rx
                  :ry ry
                  })]

         (if @edition?
           [bubble-input (merge bubble {:on-save on-save :on-stop on-stop})]
           [bubble-text edition? initial-state? common-behavior id]
           )
         ]))))

(defn get-link-path [link]
  (let [{:keys [src dst]} link
        src-b (get-bubble src)
        dst-b (get-bubble dst)
        src-pt (:center src-b)
        dst-pt (:center dst-b)
        path-str (str "M " (g/x src-pt) "," (g/y src-pt) " L " (g/x dst-pt) "," (g/y dst-pt))]
    {:key (str (:id src-b) (:id dst-b))
     :stroke-width 4
     :stroke "black"
     :fill "none"
     :d path-str}
    )
  )

(defn draw-links []
  ;; (clog (:links @points))
  (let [links-path (doall (map (fn [link] (get-link-path link)) (:links @points)))]
    [:g
     (for [path links-path]
       [:path path])
     ]
    )
  )

(defn all-bubble [svg-root]
  ;; (clog "all-bubble")
  ;; (clog @points)
  ;; (js/console.log "on-drag" on-drag)
  ;; (js/console.log @points)
  ;; (clog svg-root)
  ;; (js/console.log svg-root)
  ;; (clog (.-activeElement js/document))
  ;; (js/console.log (.-activeElement js/document))
  ;; (clog (get-in @points [:edition?-bubble :id]))
  [:g
   (draw-links)
   (draw-root-bubble svg-root)
   (doall
    (for [bubble (filter #(not= (:id %) root-id) (:bubbles @points))]
      ^{:key (:id bubble)} [draw-bubble svg-root bubble]
      )
    )
   ])
