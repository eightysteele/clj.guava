(ns clj.guava.test.base
  (:use [clj.guava.base])
  (:use [clojure.test]))

(deftest test-check-arg
  (check-arg 1)
  (is (thrown? IllegalArgumentException (check-arg nil)))
  (is (thrown? IllegalArgumentException (check-arg false))))

(deftest test-check-not-nil
  (is (= (check-not-nil 1) 1))
  (is (= "test" (check-not-nil "test")))
  (is (= '(1 2 3) (check-not-nil '(1 2 3))))
  (is (thrown? NullPointerException (check-not-nil nil))))

(deftest test-check-state
  (check-state 1)
  (is (thrown? IllegalStateException (check-state nil)))
  (is (thrown? IllegalStateException (check-state false))))

(deftest test-check-pos-index
  (is (= 1 (check-pos-index 1 2)))
  (is (= 0 (check-pos-index 0 1)))
  (is (= 1 (check-pos-index 1 1)))
  (is (thrown? IndexOutOfBoundsException (check-pos-index 3 1)))
  (is (thrown-with-msg?  IndexOutOfBoundsException #"test" (check-pos-index 3 1 "test"))))

(deftest test-check-el-index
  (is (= 1 (check-el-index 1 2)))
  (is (= 0 (check-el-index 0 1)))
  (is (= 1 (check-el-index 1 2)))
  (is (thrown? IndexOutOfBoundsException (check-el-index 3 1)))
  (is (thrown-with-msg?  IndexOutOfBoundsException #"test" (check-el-index 3 1 "test"))))

(deftest test-check-valid-options
  (check-valid-options {:a 1 :b 2 :c 3} :a :b :c :d)
  (is (thrown? IllegalArgumentException (check-valid-options {:a 1 :b 2 :d 3} :a :b :c))))

(deftest test-check-args
  (let [args [1 2 3 4]]
    (check-args
     (vector? args) "args must be a vector"
     (even? (count args)) "args count must be an even number")))

(deftest hash-code-test
  (testing "Test hash-code"
    (is (= 31 (hash-code nil)))
    (is (= 32 (hash-code 1)))
    (is (= 994 (hash-code 1 2)))
    (is (= 1794136866 (hash-code 1 2 "hello world")))))

(deftype Foo [x y] Runnable
         (run [this] (prn x y))
         (toString [this]
           (with-str-helper this
             (addv x)
             (addv y))))

(deftest test-with-str-helper-addv
  (testing "Test with-str-helper"
    (is (thrown? NullPointerException (omit-nils)))
    (is (thrown? NullPointerException (addv :a 1)))
    (is (= "Foo{1, 2}" (str (Foo. 1 2))))
    (is (= "Foo{1, 2}" (with-str-helper Foo
                         (addv 1)
                         (addv 2))))
    (is (= "Foo{x=1, y=2}" (with-str-helper (Foo. 1 2)
                             (addv :x 1)
                             (addv :y 2))))
    (is (= "Foo{x=1}" (with-str-helper (Foo. 1 nil) :as foo
                        (omit-nils)
                        (addv :x (.x foo))
                        (addv :y (.y foo)))))

    (let [foo (Foo. 3 4)]
      (is (= "Foo{x=3, y=4}" (with-str-helper foo
                               (addv :x (.x foo))
                               (addv :y (.y foo))))))
    (is (= "Foo{x=4, y=5}" (with-str-helper (Foo. 4 5) :as foo
                             (addv :x (.x foo))
                             (addv :y (.y foo)))))))



(deftest test-st-str
  (let [e (RuntimeException. "test")]
    (is (st-str e))
    (is (instance? String (st-str e)))))

(deftest test-root-cause
  (let [e (java.io.IOException. (RuntimeException. (IllegalStateException. "test")))
        cause (root-cause e)]
    (is (instance? IllegalStateException cause))
    (is (= "test" (.getMessage cause)))))

(deftest test-cause-seq
  (let [e (java.io.IOException. (RuntimeException. (IllegalStateException. "test")))
        s (cause-seq e)]
    (is (= (count s ) 3))
    (is (seq? s))
    (is (instance? java.io.IOException (first s)))
    (is (instance? RuntimeException (second s)))
    (is (instance? IllegalStateException (first (nnext s))))))


;;basic utitilis

(deftest test-exists?
  (is (exists? 1))
  (is (exists? 0))
  (is (exists? true))
  (is (exists? false))
  (is (false? (exists? nil))))

(deftest test-pearl
  (let [f (fn [x y] x)
        new-f (pearl f)]
    (is (exists? new-f))
    (is (= 1 (f 1 2)))
    (is (= 2 (new-f 1 2))))
  (let [f (fn [x y z] (+ x y))
        new-f (pearl f)]
    (is (= 3 (f 1 2 3)))
    (is (= 5 (new-f 1 2 3)))))

(deftest test-var-ns
  (is (= (symbol "clojure.test") (.name (var-ns #'deftest))))
  (is (= (symbol "clojure.core") (.name (var-ns #'contains?)))))

(deftest test-btrue?
  (is (btrue? 0))
  (is (btrue? 1))
  (is (btrue? true))
  (is (false? (btrue? nil)))
  (is (false? (btrue? false))))

(deftest test-bfalse?
  (is (bfalse? nil))
  (is (bfalse? false))
  (is (false? (bfalse? 0)))
  (is (false? (bfalse? "")))
  (is (false? (bfalse? '())))
  (is (false? (bfalse? true)))
  (is (false? (bfalse? 1))))