(ns clj.guava.test.base
  (:use [clj.guava.base])
  (:use [clojure.test]))

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
    (is (= "Foo{1, 2}" (str (Foo. 1 2))))
    (is (= "Foo{1, 2}" (with-str-helper Foo
                         (addv 1)
                         (addv 2))))
    (is (= "Foo{x=1, y=2}" (with-str-helper (Foo. 1 2)
                             (addv :x 1)
                             (addv :y 2))))
    (let [foo (Foo. 3 4)]
      (is (= "Foo{x=3, y=4}" (with-str-helper foo
                               (addv :x (.x foo))
                               (addv :y (.y foo))))))))