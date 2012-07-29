;; The usage of the eventbus is:
;; 1. create a eventbus
;;   (def bus (mk-eventbus))
;; 2. register your event handler function to the eventbus
;;   (register! bus event-name handler)
;; 3. then the event generator can post the event:
;;   (post! bus event-name event)


(ns ^{:doc "Clojure version of guava eventbus, it is totally reimplemented"
      :author "xumingmingv"}
  clj.guava.eventbus
  (:import [java.util.concurrent ConcurrentLinkedQueue])
  (:use [clojure.tools.logging :only [error]]))

(defn- dispatch [eventbus]
  "Dispatches the event to handlers."
  (when-not @(:dispatching? eventbus)
    (reset! (:dispatching? eventbus) true)
    (try
      (let [^ConcurrentLinkedQueue events (:events eventbus)]
        (while (not (empty? events))
          (let [head-event (.poll events)
                handlers (:handlers eventbus)
                event-name (:event-name head-event)
                event-obj (:event head-event)
                this-handlers (@handlers event-name)]
            (when this-handlers
              (doseq [handler this-handlers]
                ;; catch all the exceptions, so make sure one error in
                ;; a handler will not affect the whole event processing.
                (try
                  (handler event-obj)
                  (catch Throwable e
                    (error e))))))))
      (finally
       (reset! (:dispatching? eventbus) false)))))

(defn mk-eventbus
  "Creates a new eventbus with the specified name, if name not provided :default will be used."
  {:added "0.1"}
  ([]
     (mk-eventbus :default))
  ([name]
     (let [eventbus {:name name
                     :handlers (atom {})
                     :events (ConcurrentLinkedQueue.)
                     :dispatching? (atom false)}]
       eventbus)))

(defn register!
  "Register the event-handler to handle the specified event"
  {:added "0.1"}
  [eventbus event-name handler]
  (when-not (fn? handler)
    (throw (IllegalArgumentException. "event handler should be a function accepts a single param.")))
  ;; TODO check the handler is a handler
  (let [handlers (:handlers eventbus)
        this-handlers (get-in @handlers [event-name])]
    (when-not this-handlers
      (swap! handlers assoc-in [event-name] []))
    (swap! handlers update-in [event-name] conj handler)))

(defn unregister!
  "Unregiser the event-handler."
  {:added "0.1"}
  [eventbus event-name handler]
  (let [handlers (:handlers eventbus)
        this-handlers (@handlers event-name)]
    (if this-handlers
      (swap! handlers update-in [event-name] #(vec (remove #{handler} %)))
      (throw (RuntimeException. (str "No such event registered: " event-name))))))

(defn post!
  "Post a event to the specified eventbus"
  {:added "0.1"}
  [eventbus event-name event]
  (let [^ConcurrentLinkedQueue events (:events eventbus)]
    (.add events {:event-name event-name :event event})
    (dispatch eventbus)))

