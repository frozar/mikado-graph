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

(defn svg-canvas []
  (reagent/create-class
   {:display-name "svg-canvas"

    :component-did-mount
    (fn [this]
      (let [svg-bbox-client (.getBoundingClientRect (rdom/dom-node this))
            svg-origin-x (.-left svg-bbox-client)
            svg-origin-y (.-top svg-bbox-client)]
        (coord/init-svg-origin! svg-origin-x svg-origin-y)))

    :reagent-render
    (fn []
      ;; (prn "DBG camera" @event/camera)
      ;; (prn "DBG camera->viewBox" (event/camera->viewBox))
      [:svg
       {:id "svg-canvas"
        ;; :viewBox (str "0 0 " (:height @event/camera) " " (:width @event/camera))
        ;; :viewBox "0 0 1000 1000"
        :viewBox (camera/camera->viewBox)
        ;; :height 700
        ;; :width 700
        :height (.-innerHeight js/window) ;; (:height @event/camera)
        :width (.-innerWidth js/window) ;; (:width @event/camera)
        :style
        {:border "none"
         :background "#f1f1f1"
         :position "fixed"
         :top 0
         :left 0
         ;; :height "100%"
         ;; :width "100%"
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
            (let [[mouse-x mouse-y] (coord/get-svg-coord
                                     (.-clientX evt) (.-clientY evt))
                  ]
              (when (if-left-click evt)
                ;; (prn "DBG mouse-x mouse-y" mouse-x mouse-y)
                (pan/panning)
                #_(dragging bubble-id)))))
        #_(fn [evt]
            (let [[mouse-x mouse-y] (coord/get-svg-coord
                                     (.-clientX evt) (.-clientY evt))
                  ]
              (prn "DBG mouse-x mouse-y" mouse-x mouse-y))
            )
        }

       ;; This filter is used in rough display mode: background of text node
       ;; Documentation: https://stackoverflow.com/questions/15500894/background-color-of-text-in-svg
       [:defs
        [:filter
         {:x 0 :y 0 :width 1 :height 1 :id "bg-text"}
         [:feFlood {:floodColor "#ffffffee"}]
         [:feComposite {:in "SourceGraphic" :operator "xor"}]
         ]]

       [draw-graph]])}))
