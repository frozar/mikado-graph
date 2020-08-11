(ns bubble.gui-common
  (:require
   [bubble.camera :as camera]
   [bubble.constant :as const]
   [bubble.state-gui :refer [event-queue]]
   [cljs.core.async :refer [put!]]
   [clojure.string :as string]
   [reagent.core :as reagent]
   [reagent.dom :as rdom]
   ))

(defn- center-textarea
  "Center the textarea field against the surrounding bubble"
  [dom-node
   width-atom height-atom top-left-x-atom top-left-y-atom]
  (let [width  (camera/scale-dist (.-width (.getBoundingClientRect dom-node)))
        height (camera/scale-dist (.-height (.getBoundingClientRect dom-node)))
        ]
    (reset! width-atom width)
    (reset! height-atom height)
    (reset! top-left-x-atom (- 0 (/ width 2)))
    (reset! top-left-y-atom (- 0 (/ height 2)))
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
  (let [width  (camera/scale-dist (.-width (.getBoundingClientRect dom-node)))
        height (camera/scale-dist (.-height (.getBoundingClientRect dom-node)))
        radial-offset 40
        new-rx (-> width (/ 2) (+ radial-offset))
        new-ry (-> height (/ 2) (+ radial-offset))]
    (put! event-queue [:resize-bubble id new-rx new-ry])
    )
  )

(defn bubble-input
  "Create the input textarea tag to receive text updates."
  [{:keys [id rx ry text] :as bubble}]
  (let [stop
        #(put! event-queue [:disable-edition id])

        save
        (fn [text-inside-textarea]
          (let [input-text (-> text-inside-textarea str clojure.string/trim)]
            (if-not (empty? input-text)
              (put! event-queue [:save-text id input-text])
              )
            (stop)))

        current-text (reagent/atom text)
        dom-node (reagent/atom nil)

        width (reagent/atom (camera/scale-dist (* 2 rx)))
        height (reagent/atom (camera/scale-dist (* 2 ry)))
        top-left-x (reagent/atom 0)
        top-left-y (reagent/atom 0)
        ]
    (reagent/create-class
     {
      :display-name "bubble-input"

      :component-did-mount
      (fn [this]
        ;; Retrieve the textarea dom node from the foreignObject parent node
        (reset! dom-node (.-firstChild (rdom/dom-node this)))
        (.focus @dom-node)
        (center-textarea @dom-node
                         width height top-left-x top-left-y)
        (update-bubble-size @dom-node bubble)
        (cursor-to-end-textarea bubble @dom-node @current-text)
        (reset! current-text text)
        )

      :component-did-update
      (fn []
        ;; Set the focus to the textarea
        (.focus @dom-node)
        (center-textarea @dom-node
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
               (condp = (.-key evt)
                 "Enter"
                 (when (not (.-shiftKey evt))
                   (save (.. evt -target -value)))

                 "Escape"
                 (stop)

                 nil))

             }]]))})))
