(ns bubble.state-gui
  (:require
   [cljs.core.async :refer [chan]]))

(def event-queue (chan))
(def simulation? (atom true))
