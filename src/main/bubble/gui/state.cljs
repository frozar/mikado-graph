(ns bubble.gui.state
  (:require
   [cljs.core.async :refer [chan]]))

(def event-queue (chan))
(def simulation? (atom true))
