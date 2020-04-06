(ns bubble.state
  (:require
   [bubble.bubble :as bubble]
   [bubble.constant :refer [REDERING-STYLE-SOLID REDERING-STYLE-ROUGH]]
   [reagent.core :rename {atom ratom}]
   ))

(defn- init-appstate []
  {
   :bubbles [bubble/root-bubble]
   :links []
   :link-src nil
   :mouse-position nil
   :rendering-style REDERING-STYLE-ROUGH
   })

(defonce appstate
  (ratom (init-appstate)))
