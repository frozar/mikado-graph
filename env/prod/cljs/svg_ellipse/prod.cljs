(ns svg-ellipse.prod
  (:require [svg-ellipse.core :as core]))

;;ignore println statements in prod
(set! *print-fn* (fn [& _]))

(core/init!)
