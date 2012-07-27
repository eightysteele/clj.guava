;; The usage of the eventbus is:
;; 1. create a eventbus
;;   (mk-eventbus)
;; 2. register your event handler function to the eventbus
;;   (register! event-name event-handler-fn) -- there is a default eventbus, so we dont need to add the eventbus here.
;; 3. then the event generator can post the event:
;;   (post! event-name event)  -- same as above, no need to pass eventbus, use default eventbus.


(ns ^{:doc "Clojure version of guava eventbus, it is totally reimplemented"
      :author "xumingmingv"}
  clj.guava.eventbus)

;; *event-buses* is a map: the structure of it is:
;; {:default                                 -- eventbus name
;;    {                                      -- actual content of a eventbus
;;      :handlers
;;        {
;;          :window-closed                   -- event name
;;            [
;;                 window-closed-handler1    -- event handler
;;                 window-closed-handler2
;;            ]
;;        }
;;      :events
;;        {
;;            :window-closed
;;              [
;;                  {:msg "window is closed"}  -- event
;;                  {:msg "window is closed again"}
;;              ]
;;        }
;;    }
;; }

(def *event-buses* (atom {}))

#_(defn mk-eventbus
  ([]
     (mk-eventbus "default"))
  ([eventbus-name]
     (swap! *event-buses* assoc eventbus-name {:handlers {} :events {}})))

#_(defn register!
  ([event-type event-handler-fn]
     (register! :default event-type event-handler-fn))
  ([eventbus-name event-type event-handler-fn]
     (swap! *event-buses* assoc-in [:handlers event-type] event-handler-fn)))

#_(defn post! [event-name event]
  (.post eventbus event))
