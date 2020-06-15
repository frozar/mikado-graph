(ns simulation.core
  (:require
   [bubble.camera :as camera]
   [bubble.state-read :as state-read]
   [cljsjs.d3]
   [clojure.walk :as walk]
   ))

(defn appstate->graph [appstate]
  (let [nodes-field
        (reduce
         (fn [acc [id {:keys [cx cy]}]]
           (conj acc {:id id :x cx :y cy :group 1}))
         []
         (state-read/get-bubbles appstate))

        links-field
        (reduce
         (fn [acc {:keys [src dst]}]
           (conj acc {:source src :target dst :value 10}))
         []
         (state-read/get-links appstate))]
    {:nodes nodes-field
     :links links-field}))

(defn ticked [sim] ;; links nodes labels
  (fn tick []
    ;; (prn "DBG tick")
    (let [
          ;; selected-links
          ;; (-> js/d3
          ;;     (.selectAll "#app svg .links line"))
          ;; selected-nodes
          ;; (-> js/d3
          ;;     (.selectAll "#app svg .nodes circle"))
          ;; selected-labels
          ;; (-> js/d3
          ;;     (.selectAll "#app svg .labels text"))
          ]

      (.log js/console "TICK (.nodes sim)" (.nodes sim))
      (.log js/console "TICK (js->clj (.nodes sim))"
            (-> (.nodes sim)
                js->clj
                walk/keywordize-keys))

      ;; ;; (.log js/console "selection links" selected-links)
      ;; ;; (.log js/console "selection labels" selected-labels)

      ;; (-> selected-links
      ;;     (.attr "x1" (fn [d] (.. d -source -x)))
      ;;     (.attr "y1" (fn [d] (.. d -source -y)))
      ;;     (.attr "x2" (fn [d] (.. d -target -x)))
      ;;     (.attr "y2" (fn [d] (.. d -target -y))))

      ;; ;; (let [myriel
      ;; ;;       (-> (.. node -_groups)
      ;; ;;           (aget 0)
      ;; ;;           ;; (aget 0)

      ;; ;;           ;; (#(.. % -attributes #_-cx #_-value))
      ;; ;;           ;; (.getNamedItem "cx")

      ;; ;;           ;; (#(js->clj %))
      ;; ;;           ;; (#(type %))
      ;; ;;           ;; (#(.-value %))
      ;; ;;           ;; (#(type->str %))
      ;; ;;           ;; (#(js-keys %))
      ;; ;;           ;; (#(.. % -value))
      ;; ;;           )]
      ;; ;;   (.log js/console "before"
      ;; ;;         (map
      ;; ;;          #(.. % -id)
      ;; ;;          ;; #(.. % -cx -baseVal -value)
      ;; ;;          myriel))
      ;; ;;   ;; (.log js/console "before" (.. myriel -cx -baseVal -value))
      ;; ;;   ;; (prn "1" (.-name toto))
      ;; ;;   )

      ;; ;; (.log js/console "TICKED nodes" nodes)
      ;; ;; (.log js/console "TICKED nodes groups" (.. nodes -_groups))
      ;; ;; (.log js/console "filter node"
      ;; ;;       (->>  node
      ;; ;;             (filter (fn [n] (not= (.-id n) "FAKE")))
      ;; ;;             js->clj
      ;; ;;             (map
      ;; ;;              (fn [{id "id" x "x" y "y"}]
      ;; ;;                [id x y]
      ;; ;;                ))
      ;; ;;             ))

      ;; ;; (swap! app-state
      ;; ;;        assoc :tick
      ;; ;;        (->>  nodes
      ;; ;;              (filter (fn [n] (not= (.-id n) "FAKE")))
      ;; ;;              js->clj
      ;; ;;              (map
      ;; ;;               (fn [{id "id" x "x" y "y"}]
      ;; ;;                 [id x y]
      ;; ;;                 ))))

      ;; (-> selected-nodes
      ;;     (.attr "cx" (fn [d] (.-x d)))
      ;;     (.attr "cy" (fn [d] (.-y d))))

      ;; ;; (.log js/console "selection nodes" selected-nodes)
      ;; ;; (cljs.pprint/pprint [["toto" 5]])

      ;; ;; (.log js/console
      ;; ;;  ;; cljs.pprint/pprint
      ;; ;;  "selection nodes data"
      ;; ;;       ;; (-> selected-nodes
      ;; ;;       ;;     (.attr "cx"))
      ;; ;;       ;; (.attr selected-nodes "cx")
      ;; ;;       (-> (.. selected-nodes -_groups)
      ;; ;;           (aget 0)

      ;; ;;           ;; convert NodeList to clojure list
      ;; ;;           ((fn [NodeList]
      ;; ;;              (for [i (range (.-length NodeList))]
      ;; ;;                (.item NodeList i))
      ;; ;;              ))

      ;; ;;           ((fn [circles]
      ;; ;;              (mapv
      ;; ;;               (fn [circle]
      ;; ;;                 (-> circle
      ;; ;;                     (.-__data__)
      ;; ;;                     js->clj
      ;; ;;                     w/keywordize-keys
      ;; ;;                     ((fn [{:keys [id x y]}]
      ;; ;;                        [id x y]
      ;; ;;                        ))))
      ;; ;;               circles)))

      ;; ;;           ;; (#(into [] %))

      ;; ;;           )
      ;; ;;       )

      ;; ;; (.log js/console (type node))

      ;; ;; (let [myriel
      ;; ;;       (-> (.. node -_groups)
      ;; ;;           (aget 0)
      ;; ;;           (aget 0)
      ;; ;;           )]
      ;; ;;   (.log js/console "after " (.. myriel -cx -baseVal -value))
      ;; ;;   )

      ;; (-> selected-labels
      ;;     (.attr "dx" (fn [d] (.-x d)))
      ;;     (.attr "dy" (fn [d] (.-y d))))

    )))

(defn simulation [cx-svg-user cy-svg-user
                  ;; height width
                  graph] ;; links nodes labels
  (let [sim
        (-> js/d3
            (.forceSimulation)
            (.force "link"
                    (-> js/d3
                        (.forceLink)
                        (.id (fn [d] (.-id d)))
                        (.distance 100)))
                        ;; (.strength 0.7)

            (.force "charge"
                    (.forceManyBody js/d3))
            (.force "center"
                    (.forceCenter
                     js/d3
                     ;; (/ width 2)
                     ;; (/ height 2)
                     cx-svg-user
                     cy-svg-user))

            (.force "positionning"
                    (-> js/d3
                        (.forceY 100)
                        (.strength
                         (fn [d]
                           ;; (.log js/console (.-id d))
                           ;; -30
                           (if (= (.-id d) "FAKE")
                             0.99
                             0))

                         #_0.05))))

        input-nodes (.-nodes graph)
        ;; _ (.log js/console "SIM (.-nodes graph)" (.-nodes graph))
        ;; _ (prn "prn 0 (.-nodes graph)" (.-nodes graph))
        ;; fake-node (clj->js {:id "FAKE", :group 1, :index 5, :x 0, :y 0, :vx 0, :vy 0})
        ;; _ (.log js/console "nodes" nodes)
        ;; _ (.log js/console "BEFORE (.-_groups nodes)" (.. nodes -_groups))
        ;; every-nodes (.concat (.-_groups nodes) fake-node)
        ;; every-nodes input-nodes
        ;; every-nodes (.concat input-nodes fake-node)
        every-nodes input-nodes
        ;; _ (.log js/console "AFTER  (.-_groups nodes)" (.. nodes -_groups))
        ;; _ (.log js/console "AFTER  every-nodes" every-nodes)
        ;; every-nodes (.append nodes fake-node)
        ]

    ;; (.log js/console "real-nodes" real-nodes)
    ;; (.log js/console "fake-node" fake-node)
    ;; (.log js/console "every-nodes" every-nodes)

    ;; (.log js/console "graph" graph)
    ;; (.log js/console "nodes" nodes)
    ;; (.log js/console "(.-groups nodes)" (.. nodes -groups))
    ;; (.log js/console "(.-nodes graph)" (.-nodes graph))
    (.log js/console "every-nodes" every-nodes)


    ;; TODO: set the middle to the "myriel" node
    ;; TODO: add a fake node with a positive charge to simulate upward attraction

    (-> sim
        (.nodes
         ;; (.-nodes graph)
         every-nodes
         )

        (.on "tick"
             ;; (ticked links nodes labels)
             (ticked sim)
             ;; (ticked links every-nodes labels)
             ))

    ;; (.log js/console "strength set"
    ;;       (-> sim
    ;;           (.force "charge")
    ;;           (.strength
    ;;            (fn [d]
    ;;              ;; (.log js/console (.-id d))
    ;;              ;; -30
    ;;              (if (= (.-id d) "FAKE")
    ;;                5000
    ;;                -30)))))

    (.log js/console "(.nodes sim)" (.nodes sim))

    ;; (.log js/console "strength call"
    ;;       (-> sim
    ;;           (.force "charge")
    ;;           (.strength)
    ;;           ;; (.call)
    ;;           ))

    (-> sim
        (.force "link")
        (.links (.-links graph)))

    (.log js/console "(.-links graph)" (.-links graph))

    (-> sim
        (.stop))

    ;; (-> sim
    ;;     (.tick))

    ;; (.tick sim)

    ;; (-> sim
    ;;     (.restart))

    ;; (-> sim
    ;;     (.stop))

    ;; (-> sim
    ;;     (.tick))

    ;; (-> sim
    ;;     (.tick))

    ;; (prn "prn 1 (.-nodes graph)" (.-nodes graph))

    ;; (-> sim
    ;;     (.restart))

    ;; (-> sim
    ;;     (.tick))

    ;; (-> sim
    ;;     (.stop))

    ;; (prn "prn 2 (.-nodes graph)" (.-nodes graph))

    ;; (js/setTimeout
    ;;  (fn []
    ;;    (-> sim
    ;;        (.stop)))
    ;;  0.1)

    sim))

(defn launch-simulation [appstate]
  (let [graph (appstate->graph appstate)
        {:keys [cx-svg-user cy-svg-user]} (camera/state-center)]
    (simulation cx-svg-user cy-svg-user graph)))
