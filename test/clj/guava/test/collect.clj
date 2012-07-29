(ns clj.guava.test.collect
  (:refer-clojure :exclude [sort max min reverse])
  (:use [clj.guava.collect])
  (:use [clojure.test])
  (:use [clj.guava.base :only [not-nil? bfalse? btrue?]]))

(deftest test-create-ordering
  (is (not-nil? (order-by)))
  (is (not-nil? (order-by :nat)))
  (is (not-nil? (order-by :str)))
  (is (not-nil? (order-by :arb)))
  (is (not-nil? (order-by (comparator #(> %1 %2)))))
  (is (not-nil? (order-by #(- %1 %2))))
  (is (not-nil? (ordering #(> %1 %2))))
  (is (thrown? java.lang.IllegalArgumentException (explicit 1 2 3 1))) 
  (is (not-nil? (explicit 1 2 3 4))))

(deftest test-ordering-manipulation
  (let [ord (order-by)]
    (is (not-nil? ord))
    (is (not-nil? (reverse ord)))
    (is (not-nil? (compound ord #(- %1 %2))))
    (is (not-nil? (nilslast ord)))
    (is (not-nil? (nilsfirst ord)))))

(deftest test-greatest-least
  (testing "testing greatest"
    (let [ord (order-by)]
      (is (= '(5) (greatest ord '(1 2 3 4 5) 1)))
      (is (= '(5 4) (greatest ord '(1 2 3 4 5) 2)))
      (is (= '(5 4 3) (greatest ord '(1 2 3 4 5) 3)))
      (let [ord (reverse ord)]
        (is (= '(1) (greatest ord '(1 2 3 4 5) 1)))
        (is (= '(1 2) (greatest ord '(1 2 3 4 5) 2)))
        (is (= '(1 2 3) (greatest ord '(1 2 3 4 5) 3))))))
  (testing "testing least"
    (let [ord (order-by)]
      (is (= '(1) (least ord '(1 2 3 4 5) 1)))
      (is (= '(1 2) (least ord '(1 2 3 4 5) 2)))
      (is (= '(1 2 3) (least ord '(1 2 3 4 5) 3)))
      (let [ord (reverse ord)]
        (is (= '(5) (least ord '(1 2 3 4 5) 1)))
        (is (= '(5 4) (least ord '(1 2 3 4 5) 2)))
        (is (= '(5 4 3) (least ord '(1 2 3 4 5) 3)))))))

(deftest test-cmp
  (let [ord (order-by)]
    (is (< (cmp ord 1 2) 0))
    (is (thrown? NullPointerException (cmp ord nil 1)))
    (is (= (cmp ord 1 1) 0))
    (is (> (cmp ord 2 1) 0))))

(deftest test-max
  (testing "testing max"
    (let [ord (order-by)]
      (is (= 6 (max ord 5 6)))
      (is (= 6 (max ord 6)))
      (is (nil? (max ord)))
      (is (= 6 (max ord 1 2 3 4 5 6)))
      ))
  (testing "testing max*"
    (let [ord (order-by :str)]
      (is (= 2 (max* ord '(1 2 11 12 13))))
      (is (thrown? NullPointerException (max* ord '()))))))


(deftest test-min
  (testing "testing min"
    (let [ord (order-by)]
      (is (= 5 (min ord 5 6)))
      (is (= 6 (min ord 6)))
      (is (nil? (min ord)))
      (is (= 1 (min ord 1 2 3 4 5 6)))
      ))
  (testing "testing min*"
    (let [ord (order-by :str)]
      (is (= 1 (min* ord '(1 2 11 12 13))))
      (is (= 0 (min* ord '(9 8 2 3 5 0 7))))
      (is (thrown? NullPointerException (min* ord '()))))))

(deftest test-sort-ordered?
  (let [ord1 (order-by)
        ord2 (-> (order-by) (reverse))
        ls (shuffle (take 10 (iterate inc 1)))]
    (is (false? (ordered? ord1 ls)))
    (is (= '(1 2 3 4 5 6 7 8 9 10) (sort ord1 ls)))
    (is (not= ls (sort ord1 ls)))
    (is (ordered? ord1 (sort ord1 ls)))
    (is (strictly-ordered? ord1 (sort ord1 ls)))
    (is (false? (ordered? ord2 (sort ord1 ls))))
    (is (false? (ordered? ord2 ls)))
    (is (false? (ordered? ord1 ls)))
    (is (= '(10 9 8 7 6 5 4 3 2 1)  (sort ord2 ls)))
    (is (false? (strictly-ordered? ord1 '(1 2 2 3 4 5 6 7 8 9))))
    (is (ordered? ord1 '(1 2 2 3 4 5 6 7 8 9)))
    (is (strictly-ordered? ord1 '(1 2 3 4 5 6 7 8 9)))
    (let [ls (sort ord1 ls)]
      (is (= 9 (search ord1 ls 10)))
      (is (= 2 (search ord1 ls 3)))
      (is (= -11 (search ord1 ls 11)))
      (is (= -11 (search ord1 ls 99)))
      )))

(deftest test-include?
  (is (include? [4 5 6 7] 5))
  (is (include? '(4 5 6 7) 5))
  (is (bfalse? (include? [4 5 6] 0)))
  (is (include? {:a 1 :b 2} :a))
  (is (include? #{5 6 3 4} 3))
  (let [m (doto (java.util.HashMap.) (.put "a" 1) (.put "b" 2))
        ls (doto (java.util.ArrayList.) (.add 1) (.add 2) (.add "hello"))]
    (is (include? m "a"))
    (is (include? m "b"))
    (is (bfalse? (include? m "c")))
    (is (bfalse? (include? m "d")))
    (is (include? ls 1))
    (is (include? ls 2))
    (is (include? ls "hello"))
    (is (bfalse? (include? ls "world")))
    ))

(deftest test-with-ordering
  (testing "with-ordering"
    (is (thrown? NullPointerException (cmp 1 2))) 
    (with-ordering (order-by)
      (is (ordered? '(1 2 3 4)))
      (is (< (cmp 1 2) 0))
      (is (= '(1 2 3 4) (sort '(3 4 1 2)))))))