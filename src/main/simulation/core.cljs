(ns simulation.core
  (:require
   [bubble.camera :as camera]
   [bubble.state-read :as state-read]
   [cljs.core.async :refer [put!]]
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

(defn ticked [event-queue sim]
  (fn tick []
    (let [nodes (-> (.nodes sim)
                    js->clj
                    walk/keywordize-keys)
          ]

      ;; (.log js/console "TICK (.nodes sim)" (.nodes sim))
      ;; (.log js/console "TICK (js->clj (.nodes sim))"
      ;;       nodes)

      (put! event-queue [:simulation-move nodes])

    )))

(defn simulation [event-chan
                  cx-svg-user cy-svg-user
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
    (.log js/console "input-nodes" input-nodes)
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
             (ticked event-chan sim)
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

    ;; (-> sim
    ;;     (.stop))

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

(defn launch-simulation [appstate event-queue]
  (let [graph (appstate->graph appstate)
        {:keys [cx cy]} (camera/state-center)
        sim (simulation event-queue cx cy (clj->js graph))]))
