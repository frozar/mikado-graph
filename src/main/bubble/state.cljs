(ns bubble.state
  (:require
   [bubble.bubble :as bubble]
   [reagent.core :rename {atom ratom}]
   ))

(defn- init-appstate []
  {
   :bubbles [bubble/root-bubble]
   :links []
   :link-src nil
   :mouse-position nil
   })

(defonce appstate
  (ratom (init-appstate)))
