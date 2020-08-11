(ns bubble.state-gui
  (:require
   [cljs.core.async :refer [chan]]))

(js/console.log "LOADING bubble.state-gui")
(def event-queue (chan))
(def simulation? (atom true))
(js/console.log "@simulation? " @simulation?)
