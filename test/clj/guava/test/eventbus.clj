(ns clj.guava.test.eventbus
  (:use [clojure.test])
  (:use [clj.guava.eventbus]))

(deftest test-mk-eventbus
  (let [bus (mk-eventbus)]
    (is (= false (nil? bus)))
    (is (= false (nil? (:handlers bus))))
    (is (= false (nil? (:events bus))))
    (is (= :default (:name bus))))
  (let [bus (mk-eventbus "test")]
    (is (= false (nil? bus)))
    (is (= "test" (:name bus)))))

(deftest test-register!
  (let [bus (mk-eventbus)
        handlers (:handlers bus)
        fn1 (fn [str] str)
        fn2 (fn [str] (println str))]
    (register! bus "event" fn1)
    (register! bus "event" fn2)
    (is (= [fn1 fn2] (vec (@handlers "event"))))
    (register! bus "event1" fn1)
    (is (= [fn1] (vec (@handlers "event1"))))
    (register! bus "event1" fn2)
    (is (= [fn1 fn2] (vec (@handlers "event1"))))))

(deftest test-register!-not-fn
  (let [bus (mk-eventbus)
        has-error? (atom false)]
    (try
      (register! bus "event" "not-a-handler")
      (catch IllegalArgumentException e
        (reset! has-error? true)))
    (is (= true @has-error?))))

(deftest test-unregister!
  (let [bus (mk-eventbus)
        handlers (:handlers bus)
        fn1 (fn [str] str)
        fn2 (fn [str] (println str))
        fn3 (fn [str] (println "hello"))]
    (register! bus "event" fn1)
    (register! bus "event" fn2)
    (register! bus "event" fn3)
    (register! bus "event1" fn1)

    (is (= [fn1 fn2 fn3] (vec (@handlers "event"))))
    (is (= [fn1] (vec (@handlers "event1"))))
    (unregister! bus "event" fn2)

    (is (= [fn1 fn3] (vec (@handlers "event"))))
    (unregister! bus "event" fn1)
    (is (= [fn3] (vec (@handlers "event"))))
    (unregister! bus "event" fn3)
    (is (= true (empty? (@handlers "event"))))
    (is (= [fn1] (vec (@handlers "event1"))))
    ))

(defn mk-string-catcher [strings]
  (fn [str]
    (swap! strings conj str)))

(deftest test-post!
  (let [bus (mk-eventbus)
        strings (atom [])
        string-catcher (mk-string-catcher strings)]
    (register! bus "event" string-catcher)
    (post! bus "event" "msg1")
    (is (= ["msg1"] @strings))
    (post! bus "event" "msg2")
    (is (= ["msg1" "msg2"] @strings))))

(deftest test-mutiple-handlers
  (let [bus (mk-eventbus)
        strings1 (atom [])
        string-catcher1 (mk-string-catcher strings1)
        strings2 (atom [])
        string-catcher2 (mk-string-catcher strings2)]
    (register! bus "event" string-catcher1)
    (register! bus "event" string-catcher2)
    (post! bus "event" "msg1")
    (post! bus "event" "msg2")
    (is (= ["msg1" "msg2"] @strings1))
    (is (= ["msg1" "msg2"] @strings2))))

(deftest test-multiple-events
  (let [bus (mk-eventbus)
        strings (atom [])
        string-catcher (mk-string-catcher strings)]
    (register! bus "event" string-catcher)
    (register! bus "event1" string-catcher)
    (post! bus "event" "msg1")
    (post! bus "event1" "msg2")
    (post! bus "event" "msg3")
    (post! bus "event1" "msg4")
    (is (= ["msg1" "msg2" "msg3" "msg4"] @strings))))

(defn ill-handler [str]
  (throw (RuntimeException. "from ill-handler")))

(deftest test-exception
  (let [bus (mk-eventbus)
        has-error? (atom false)]
    (try
      (register! bus "event" ill-handler)
      (catch Throwable e
        (reset! has-error? true)))
    (is (= false @has-error?))))
