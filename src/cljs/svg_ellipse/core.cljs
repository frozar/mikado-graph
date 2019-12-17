(ns svg-ellipse.core
  (:require [reagent.core :as reagent]
            [reagent.session :as session]
            [reitit.frontend :as reitit]
            [clerk.core :as clerk]
            [accountant.core :as accountant]
            [svg-ellipse.bubble :as b]
            [debux.cs.core :as d :refer-macros [clog clogn dbg dbgn break]]
            )
  )

;; -------------------------
;; Routes

(def router
  (reitit/router
   [["/" :index]
    ["/items"
     ["" :items]
     ["/:item-id" :item]]
    ["/about" :about]]))

(defn path-for [route & [params]]
  (if params
    (:path (reitit/match-by-name router route params))
    (:path (reitit/match-by-name router route))))

(path-for :about)
;; -------------------------
;; Page components

(defn home-page []
  nil)
  ;; (fn []
  ;;   [:span.main
  ;;    [:h1 "Welcome to svg_ellipse"]
  ;;    [:ul
  ;;     [:li [:a {:href (path-for :items)} "Items of svg_ellipse"]]
  ;;     [:li [:a {:href "/broken/link"} "Broken link"]]]]))



(defn items-page []
  (fn []
    [:span.main
     [:h1 "The items of svg_ellipse"]
     [:ul (map (fn [item-id]
                 [:li {:name (str "item-" item-id) :key (str "item-" item-id)}
                  [:a {:href (path-for :item {:item-id item-id})} "Item: " item-id]])
               (range 1 60))]]))


(defn item-page []
  (fn []
    (let [routing-data (session/get :route)
          item (get-in routing-data [:route-params :item-id])]
      [:span.main
       [:h1 (str "Item " item " of svg_ellipse")]
       [:p [:a {:href (path-for :items)} "Back to the list of items"]]])))


(defn about-page []
  (fn [] [:span.main
          [:h1 "About svg_ellipse"]]))


;; -------------------------
;; Translate routes -> page components

(defn page-for [route]
  (case route
    :index #'home-page
    :about #'about-page
    :items #'items-page
    :item #'item-page))


;; -------------------------
;; Page mounting component

(defn root [svg-root]
  ;; (js/console.log "root")
  ;; (js/console.log svg-root)
  ;; [:<>
   [b/all-bubble svg-root]
   ;; [b/all-bubble2 svg-root]
   ;; ]
  )

(defn svg-canvas []
    [:svg {:style {:border "1px solid"
                   :background "white"
                   :width "800"
                   :height "800"}
           :on-context-menu (fn [evt] (.preventDefault evt))
           }
     ;; (clog "svg-canvas")
     [root (reagent/current-component)]
     ])

(defn current-page []
  (fn []
    (let [page (:current-page (session/get :route))]
      ;; (clog "current-page")
      [:div
      ;;  [:header
      ;;   [:p [:a {:href (path-for :index)} "Maison"] " | "
      ;;    [:a {:href (path-for :about)} "About svg_ellipse"]]]
       [svg-canvas]
       [page]
       ;; [:footer
       ;;  [:p "svg_ellipse was generated by the "
       ;;   [:a {:href "https://github.com/reagent-project/reagent-template"} "Reagent Template"] "."]]
       ])))

;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (clerk/initialize!)
  (accountant/configure-navigation!
   {:nav-handler
    (fn [path]
      (let [match (reitit/match-by-path router path)
            current-page (:name (:data  match))
            route-params (:path-params match)]
        (reagent/after-render clerk/after-render!)
        (session/put! :route {:current-page (page-for current-page)
                              :route-params route-params})
        (clerk/navigate-page! path)
        ))
    :path-exists?
    (fn [path]
      (boolean (reitit/match-by-path router path)))})
  (accountant/dispatch-current!)
  (mount-root))
