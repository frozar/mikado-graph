(ns mikado.app
  (:require ;; [reagent.session :as session]
            ;; [reitit.frontend :as reitit]
            ;; [clerk.core :as clerk]
            ;; [accountant.core :as accountant]
            [bubble.core :as bubble]
            [reagent.dom :as rdom]
            )
  )

;; TODO: put in place frontend router

(defn graphe-page []
  (fn []
    [bubble/svg-canvas]
    ))

(defn current-page []
  [:h1 "Hello"]
  )

(defn mount-root [component]
  (rdom/render [component] (.getElementById js/document "app"))
  )

(defn ^:dev/after-load reload! []
  (mount-root graphe-page)
  )

(defn init! []
  (reload!)
  )
