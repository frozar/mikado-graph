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
                    walk/keywordize-keys)]
      (put! event-queue [:simulation-move nodes])
    )))

(defn simulation [event-chan
                  cx-svg-user cy-svg-user
                  graph]
  (let [sim
        (-> js/d3
            (.forceSimulation)
            (.force "link"
                    (-> js/d3
                        (.forceLink)
                        (.id (fn [d] (.-id d)))
                        (.distance 300)
                        (.strength 0.05)
                        ))
            (.force "charge"
                    (->  js/d3
                         (.forceManyBody)
                         (.strength -750)
                         (.distanceMin 0)))
            (.force "center"
                    (.forceCenter
                     js/d3
                     ;; (/ width 2)
                     ;; (/ height 2)
                     cx-svg-user
                     cy-svg-user))
            )
        ]

    (-> sim
        (.nodes
         (.-nodes graph))
        (.on "tick"
             (ticked event-chan sim)))

    (-> sim
        (.force "link")
        (.links (.-links graph)))

    sim))

(defn launch-simulation [appstate event-queue]
  (let [graph (appstate->graph appstate)
        {:keys [cx cy]} (camera/state-center)]
    (simulation event-queue cx cy (clj->js graph))))
