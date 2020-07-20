(ns bubble.rough-cljs
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.dom :as rdom]
            ["roughjs/bundled/rough.esm" :as RoughJSExports]
            ))

(def roughjs (.-default RoughJSExports))

;;; De-structuring primitives and drawing them
;;; See RoughJS documentation for individual calls

(defn draw-rectangle! [rough primitive opts]
  (let [[x y width height] primitive]
    (.rectangle rough x y width height opts)
    )
  )

(defn draw-circle! [rough primitive opts]
  (let [[x y radius] primitive]
    (.circle rough x y radius opts)
    )
  )

(defn draw-line! [rough primitive opts]
  (let [[x1 y1 x2 y2] primitive]
    (.line rough x1 y1 x2 y2 opts)
    )
  )

(defn draw-ellipse! [rough primitive opts]
  (let [[x y width height] primitive]
    (.ellipse rough x y width height opts)
    )
  )

(defn draw-linearPath! [rough primitive opts]
  (let [paths (clj->js primitive)]
    (.linearPath rough paths opts)
    )
  )

(defn draw-polygon! [rough primitive opts]
  (let [paths (clj->js primitive)]
    (.polygon rough paths opts)
    )
  )

(defn draw-arc! [rough primitives opts]
  (let [[x, y, width, height, start, stop, closed] primitives]
    (.arc rough x y width height start stop closed opts)
    )
  )

(defn draw-curve! [rough primitive opts]
  (let [points (clj->js primitive)]
    (.curve rough points opts)
    )
  )

(defn draw-path! [rough primitive opts]
  (let [path primitive]
    (.path rough path opts)
    )
  )

(def primitive->draw-fn {:rectangle draw-rectangle!
                         :circle    draw-circle!
                         :line      draw-line!
                         :ellipse   draw-ellipse!
                         :linearPath  draw-linearPath!
                         :polygon   draw-polygon!
                         :arc       draw-arc!
                         :curve     draw-curve!
                         :path      draw-path!
                         })

;; defmulti is slow on CLJS so we'll do our own dispatch
(defn draw! [rough primitive]
  (if-let [draw-fn (get primitive->draw-fn (first primitive))]
    (draw-fn rough (second primitive) (some-> (get primitive 2) (clj->js)))
    (println "Unknown primitive" (first primitive))
    )
  )

(defn draw-all! [rough primitives]
  (map #(draw! rough %) primitives)
  )

(defn clean-element! [dom-element]
  (-> (.getContext dom-element "2d")
      (.clearRect 0 0 (.-width dom-element) (.-height dom-element))
      )
  )

(defn remove-all-g-elements! [svg-element]
  (-> (.querySelectorAll svg-element "g")
      (.forEach (fn [n] (.removeChild (.-parentNode n) n)))
      )
  )

(defn add-to-dom!
  "Add an element to the DOM, return the added element"
  [dom-element element]
  (.appendChild dom-element element)
  element
  )

(defprotocol RoughJS
  (clean-element [this dom-element])
  (draw [this dom-element primitives])
  )

;; Canvas variant: cleaning clears the canvas, drawing just draws directly into the canvas
(defrecord rough-with-canvas [roughjs]
  RoughJS
  (clean-element [_ dom-element]
    (clean-element! dom-element)
    )
  (draw [this _ primitives]
    (dorun (draw-all! (:roughjs this) primitives))
    )
  )

;; SVG variant: cleaning removes all <g> tags, and SVG elements are added to the DOM
(defrecord rough-with-svg [roughjs]
  RoughJS
  (clean-element [_ dom-element]
    ;; (.log js/console "DBG 0 " dom-element)
    ;; (.log js/console "DBG 1 " (remove-all-g-elements! dom-element))
    (remove-all-g-elements! dom-element)
    )
  (draw [this dom-element primitives]
    (->> primitives
         (draw-all! (:roughjs this))
         (map #(add-to-dom! dom-element %))
         dorun
         )
    )
  )

(defn init-roughjs!
  "Pick the correct variant of RoughJS"
  [opts dom-node]
  (if (contains? opts :svg)
    (->rough-with-svg (.svg roughjs dom-node))
    (->rough-with-canvas (.canvas roughjs dom-node))
    )
  )

(defn Rough
  "React class to draw elements in a 'rough' look. Using RoughJS behind the scenes.
   Opts - options
     {:svg {:width :height ...}} - creates a SVG drawing, map holds props added to SVG tag
     {:canvas {:width :height ..}} - creates a canvas drawing (which is the default), map holds props added to canvas tag
   primitives -  collection of drawing primitives in the format [<type> <params> (<options>)]:
   <options> are identical for each element. See RoughJS options for description. Map is sent through clj->js and give to RoughJS
   Supported primitives:
     [:rectangle [x y width height]]
     [:circle [x y radius]]
     [:line [x1 y1 x2 y2]]
     [:ellipse [x y width height]]
     [:linearPath [[x1 y1] [x2 y2] ...]
     [:polygon [[x1 y1] [x2 y2] ...]
     [:arc [x y width height start-angle stop-angle closed?]]
     [:curve [[x1 y1] [x2 y2] ...]
     [:path \"SVG-Path-Specification\"]
   Example:
   [:div [RoughJS {:canvas {:width 200 :height 200}} [[:rectangle [0 0 10 10] [:circle [10 10 5]]]]
   Notes: Only changes to the primitives parameter are reflected, i.e. changing from svg to canvas is not possible at the moment
  "
  ([primitives]
   (Rough {} primitives)
   )
  ([opts primitives]
   (let [state (atom {})]
     (reagent/create-class
       {:display-name "RoughJS"
        :component-did-mount
        (fn [this]
          (let [;; _ (.log js/console (rdom/dom-node this))
                rough (init-roughjs! opts (rdom/dom-node this))
                dom-node (rdom/dom-node this)
                ]
            ;; (.log js/console "DBG BEFORE DRAW")
            ;; (.log js/console (rdom/dom-node this))
            ;; (.log js/console rough)
            ;; (.log js/console "DBG 0 dom this " (rdom/dom-node this))
            (draw rough (rdom/dom-node this) primitives)
            ;; (.log js/console "DBG AFTER  DRAW")
            ;; ;; (.log js/console "this " this)
            ;; (.log js/console "DBG 1 dom this " (rdom/dom-node this))
            ;; (.log js/console "js-keys dom this " (js-keys (rdom/dom-node this)))
            ;; (.log js/console "first dom this " (.-firstElementChild (rdom/dom-node this)))
            ;; (.log js/console "rough " rough)
            ;; (.log js/console "dom-node type" (.-nodeType dom-node))
            ;; (.log js/console "inner dom this " (.-innerHTML (rdom/dom-node this)))
            ;; (.log js/console "BEFORE outer dom-node " (.-outerHTML dom-node))
            ;; (set! (.-outerHTML dom-node) (.-innerHTML (rdom/dom-node this)))
            ;; (.log js/console "AFTER  outer dom-node " (.-outerHTML dom-node))
            ;; (.log js/console "(reagent/argv this) " (reagent/argv this))
            ;; (.log js/console "dom-node " dom-node)
            ;; (.log js/console "first inner dom this " (.-firstElementChild (.-innerHTML (rdom/dom-node this))))
            ;; (.log js/console "type innerHTML" (.-nodeType (.-innerHTML (rdom/dom-node this))))
            ;; (.log js/console "type dom-node " (.-nodeType dom-node))
            ;; (.log js/console "attributes dom-node " (.-attributes dom-node))
            ;; ((.setAttributes dom-node) "toto" 42)
            (swap! state assoc :roughjs rough))
          )

        :component-did-update
        (fn [this _]   ;; reagent provides you the entire "argv", not just the "props"
          (let [new-argv (rest (reagent/argv this))
                rough    (:roughjs @state)
                dom-node (rdom/dom-node this)
                ]
            ;; (.log js/console "DBG BEFORE CLEAN")
            ;; (.log js/console "(reagent/argv this) " (reagent/argv this))
            (clean-element rough (rdom/dom-node this))
            (draw rough (rdom/dom-node this) (second new-argv))
            ;; (defn remove-all-g-elements! [svg-element]
            ;;   (-> (.querySelectorAll svg-element "g")
            ;;       (.forEach (fn [n] (.removeChild (.-parentNode n) n)))
            ;;       )
            ;;   )
            ;; (.log js/console "(.querySelectorAll svg-element g)"
            ;;       (.querySelectorAll (rdom/dom-node this) "g"))
            ;; (.log js/console "next step "
            ;;       (-> (.querySelectorAll (rdom/dom-node this) "g")
            ;;           (.forEach (fn [n] (.removeChild (.-parentNode n) n)))))
            ;; (.log js/console "dom-node " dom-node)
            ;; (.log js/console "(rdom/dom-node this) " (rdom/dom-node this))
            ;; (.log js/console "DBG AFTER  CLEAN")
            ;; (.log js/console "(.-innerHTML dom-node) " (.-innerHTML dom-node))
            ;; (.log js/console "(.-outerHTML dom-node) " (.-outerHTML dom-node))
            ;; (.log js/console "(second new-argv) " (second new-argv))
            ;; (.log js/console "this " this)
            ;; (set! this (.firstChild dom-node))
            ;; (set! (.-outerHTML dom-node) (.-innerHTML (rdom/dom-node this)))
            ;; todo: switch other parameters
            ;; (println new-argv old-argv)
            ;; (swap! state assoc :roughjs rough)
            ))

        :reagent-render
        (fn [{:keys [canvas svg] :as opts} _]
          ;; (.log js/console "DBG reagent-render " (contains? opts :svg))
          ;; (.log js/console "DBG opts " opts)
          ;; (.log js/console "DBG svg " svg)
          ;; [:div]
          ;; [:text "TOTO"]
          #_(if (contains? opts :svg)
            [:svg svg]
            [:canvas canvas]
            )
          [:g]
          )}))))
