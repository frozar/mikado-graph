(ns bubble.core
  (:require
   [bubble.constant :as const]
   [bubble.coordinate :as coord]
   [bubble.event :as event]
   [bubble.gui-solid :as gui-solid]
   [bubble.gui-rough :as gui-rough]
   [bubble.state-read :as state-read]
   [reagent.core :as reagent]
   ))

(defn- draw-graph [rendering-style]
  (condp = rendering-style
    const/REDERING-STYLE-SOLID
    [:g
     {:id "graph"}
     ;; Interactive part
     (when (state-read/get-link-src)
       [gui-solid/draw-building-link
        (-> (state-read/get-link-src) state-read/get-bubble)
        (state-read/get-mouse-position)])

     ;; Static part
     (let [couples_bubble
           (map
            (fn [link]
              [(-> link :src state-read/get-bubble)
               (-> link :dst state-read/get-bubble)])
            (state-read/get-links))]
       [gui-solid/draw-links couples_bubble])
     [gui-solid/draw-bubbles (state-read/get-bubbles)]]

    const/REDERING-STYLE-ROUGH
    [:g
     {:id "graph"}
     ;; Interactive part
     (when (state-read/get-link-src)
       [gui-rough/draw-building-link
        (-> (state-read/get-link-src) state-read/get-bubble)
        (state-read/get-mouse-position)])

     ;; Static part
     (let [couples_bubble
           (map
            (fn [link]
              [(-> link :src state-read/get-bubble)
               (-> link :dst state-read/get-bubble)])
            (state-read/get-links))]
       [gui-rough/draw-links couples_bubble])
     [gui-rough/draw-bubbles (state-read/get-bubbles)]]
    ))

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
        (event/prevent-context-menu)
        }
       [draw-graph (state-read/get-rendering-style)]
       ])}))
