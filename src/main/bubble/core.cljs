(ns bubble.core
  (:require
   [bubble.camera :as camera]
   [bubble.constant :as const]
   [bubble.coordinate :as coord]
   [bubble.event :as event]
   [bubble.gui-solid :as gui-solid]
   [bubble.gui-rough :as gui-rough]
   [bubble.pan :as pan]
   [bubble.state-read :as state-read]
   [reagent.core :as reagent]
   [reagent.dom :as rdom]
   ))

(defn- which-renderer [rendering-style]
  (condp = rendering-style
    const/REDERING-STYLE-SOLID
    [gui-solid/draw-building-link
     gui-solid/draw-links
     gui-solid/draw-bubbles]

    const/REDERING-STYLE-ROUGH
    [gui-rough/draw-building-link
     gui-rough/draw-links
     gui-rough/draw-bubbles]
    )
  )

(defn- draw-graph []
  (let [[draw-building-link draw-links draw-bubbles]
        (which-renderer (state-read/get-rendering-style))]
    [:g
     {:id "graph"}
     ;; Interactive part
     (when (state-read/get-link-src)
       [draw-building-link
        (-> (state-read/get-link-src) state-read/get-bubble)
        (state-read/get-mouse-position)])

     ;; Static part
     (let [couples_bubble
           (map
            (fn [link]
              [(-> link :src state-read/get-bubble)
               (-> link :dst state-read/get-bubble)])
            (state-read/get-links))]
       [draw-links couples_bubble])
     [draw-bubbles (state-read/get-bubbles)]]))

(defn svg-origin []
  [:circle
   {:cx 0 :cy 0 :r 5 :fill "black"}])

(defn camera-center []
  (let [{:keys [cx cy]} @camera/camera]
    [:circle
     {:cx cx :cy cy :r 5 :fill "red"}]))

(defn svg-canvas []
  (reagent/create-class
   {:display-name "svg-canvas"

    :component-did-mount
    (fn [this]
      (let [svg-bbox-client (.getBoundingClientRect (rdom/dom-node this))
            svg-origin-x-px (.-left svg-bbox-client)
            svg-origin-y-px (.-top svg-bbox-client)]
        (coord/init-svg-origin! svg-origin-x-px svg-origin-y-px)))

    :reagent-render
    (fn []
      [:svg
       {:id "svg-canvas"
        :viewBox (camera/camera->viewBox)
        :height (.-innerHeight js/window)
        :width (.-innerWidth js/window)
        :style
        {:border "none"
         :background "#f1f1f1"
         :position "fixed"
         :top 0
         :left 0
         }

        :on-context-menu
        (event/prevent-default)

        :on-drag-start
        (event/prevent-default)

        :on-mouse-down
        (let [if-left-click
              (fn [evt]
                (= 0 (.-button evt)))]
          (fn [evt]
            (when (if-left-click evt)
              (pan/panning))))
        }

       ;; This filter is used in rough display mode: background of text node
       ;; Documentation: https://stackoverflow.com/questions/15500894/background-color-of-text-in-svg
       [:defs
        [:filter
         {:x 0 :y 0 :width 1 :height 1 :id "bg-text"}
         [:feFlood {:floodColor "#ffffffee"}]
         [:feComposite {:in "SourceGraphic" :operator "xor"}]
         ]]

       [draw-graph]

       ;; ;; DBG element
       ;; [svg-origin]
       ;; [camera-center]
       ])}))
