(ns bubble.event-state
  )

(js/console.log "LOADING bubble.event-state")
;; Store the settings if simulation is enable or not
(def simulation? (atom true))
(js/console.log "@simulation? " @simulation?)
