(ns bubble.state
  (:require
   [bubble.bubble :as bubble]
   [bubble.constant :refer [REDERING-STYLE-SOLID ROOT-BUBBLE-ID]]
   [reagent.core :rename {atom ratom}]
   ))

(defn- init-appstate []
  {:bubbles {ROOT-BUBBLE-ID bubble/root-bubble}
   :links []
   :link-src nil
   :mouse-position nil
   :rendering-style REDERING-STYLE-SOLID})

(defonce appstate
  (ratom (init-appstate)))
