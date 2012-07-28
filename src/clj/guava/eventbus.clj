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

;; event-buses is a map: the structure of it is:
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

(def event-buses (atom {}))

(defn- add-handler! [eventbus-name event-name event-handler-fn]
  (when-not (get-in @event-buses [eventbus-name :handlers event-name])
    (swap! event-buses assoc-in [eventbus-name :handlers event-name] []))
  (let [handler-cnt (count (get-in @event-buses [eventbus-name :handlers event-name]))]
    (swap! event-buses assoc-in [eventbus-name :handlers event-name handler-cnt] event-handler-fn)))

(defn- remove-handler! [eventbus-name event-name event-handler-fn]
  ;; check whether the eventbus exists
  (when-not (get-in @event-buses [eventbus-name :handlers event-name])
    (throw (RuntimeException. "There is no such event named: " event-name)))
  (let [handler-cnt (count (get-in @event-buses [eventbus-name :handlers event-name]))]
    (swap! event-buses update-in [eventbus-name :handlers event-name handler-cnt] event-handler-fn)))

(defn add-event! [eventbus-name event-name event]
  (when-not (get-in @event-buses [eventbus-name :events event-name])
    (swap! event-buses assoc-in [eventbus-name :events event-name] []))
  (let [handler-cnt (count (get-in @event-buses [eventbus-name :events event-name]))]
    (swap! event-buses assoc-in [eventbus-name :events event-name handler-cnt] event)))

(defn dispatch-event! [eventbus-name]
  (let [events (get-in @event-buses [eventbus-name :events])
        handlers (get-in @event-buses [eventbus-name :handlers])]
    (doseq [[event-name this-events] events
            :let [this-event-handlers (handlers event-name)]]
      (doseq [handler this-event-handlers]
        (doseq [event this-events]
          (handler event))))))


(defn- mk-eventbus [eventbus-name]
  (swap! event-buses assoc eventbus-name {:handlers {} :events {}}))

(defn- eventbus-exists? [eventbus-name]
  "Checks whether the eventbus already exists."
  (not (nil? (@event-buses eventbus-name))))

;; create the default eventbus
(mk-eventbus :default)

(defn register!
  "Register the event-handler to handle the specified event"
  ([event-name event-handler-fn]
     (register! :default event-name event-handler-fn))
  ([eventbus-name event-name event-handler-fn]
     ;; create the eventbus if it not exists yet
     (when-not (eventbus-exists? eventbus-name)
       (mk-eventbus event-name))
     (add-handler! eventbus-name event-name event-handler-fn)))

(defn unregister!
  ""
  ([event-name event-handler-fn]
     (unregister! :default event-name event-handler-fn))
  ([eventbus-name event-name event-handler-fn]
     (when-not (eventbus-exists? eventbus-name)
       (throw (RuntimeException. "There is no such eventbus named: " eventbus-name)))
     (remove-handler! eventbus-name event-name event-handler-fn)))

(defn post!
  "Post a event to the specified eventbus"
  ([event-name event]
     (post! :default event-name event))
  ([eventbus-name event-name event]
     (when-not (eventbus-exists? eventbus-name)
       (throw (RuntimeException. "There is no eventbus named: " eventbus-name)))
     (add-event! eventbus-name event-name event)
     (dispatch-event! :default)))
