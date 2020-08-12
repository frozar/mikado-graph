(ns bubble.camera-state
  (:require
   [cljs.core.async :refer [chan]]
   ))

(def event-queue (chan))
