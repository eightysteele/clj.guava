(ns clj.guava.test.collect
  (:use [clj.guava.collect])
  (:use [clojure.test])
  (:use [clj.guava.base :only [exists? bfalse? btrue?]]))

(deftest test-create-ordering
  (is (exists? (ord-by)))
  (is (exists? (ord-by :nat)))
  (is (exists? (ord-by :str)))
  (is (exists? (ord-by :arb)))
  (is (exists? (ord-by (comparator #(> %1 %2)))))
  (is (exists? (ord-by #(- %1 %2))))
  (is (exists? (ordering #(> %1 %2))))
  (is (thrown? java.lang.IllegalArgumentException (ord-explicit 1 2 3 1))) 
  (is (exists? (ord-explicit 1 2 3 4))))

(deftest test-ordering-manipulation
  (let [ord (ord-by)]
    (is (exists? ord))
    (is (exists? (ord-reverse ord)))
    (is (exists? (ord-compound ord #(- %1 %2))))
    (is (exists? (ord-nilslast ord)))
    (is (exists? (ord-nilsfirst ord)))))

(deftest test-ord-greatest-least
  (testing "testing ord-greatest"
    (let [ord (ord-by)]
      (is (= '(5) (ord-greatest ord '(1 2 3 4 5) 1)))
      (is (= '(5) (ord-greatest ord '(1 2 3 4 5))))
      (is (= '(5 4) (ord-greatest ord '(1 2 3 4 5) 2)))
      (is (= '(5 4 3) (ord-greatest ord '(1 2 3 4 5) 3)))
      (let [ord (ord-reverse ord)]
        (is (= '(1) (ord-greatest ord '(1 2 3 4 5) 1)))
        (is (= '(1) (ord-greatest ord '(1 2 3 4 5))))
        (is (= '(1 2) (ord-greatest ord '(1 2 3 4 5) 2)))
        (is (= '(1 2 3) (ord-greatest ord '(1 2 3 4 5) 3))))))
  (testing "testing ord-least"
    (let [ord (ord-by)]
      (is (= '(1) (ord-least ord '(1 2 3 4 5) 1)))
      (is (= '(1) (ord-least ord '(1 2 3 4 5))))
      (is (= '(1 2) (ord-least ord '(1 2 3 4 5) 2)))
      (is (= '(1 2 3) (ord-least ord '(1 2 3 4 5) 3)))
      (let [ord (ord-reverse ord)]
        (is (= '(5) (ord-least ord '(1 2 3 4 5) 1)))
        (is (= '(5) (ord-least ord '(1 2 3 4 5))))
        (is (= '(5 4) (ord-least ord '(1 2 3 4 5) 2)))
        (is (= '(5 4 3) (ord-least ord '(1 2 3 4 5) 3)))))))

(deftest test-ord-cmp
  (let [ord (ord-by)]
    (is (< (ord-cmp ord 1 2) 0))
    (is (thrown? NullPointerException (ord-cmp ord nil 1)))
    (is (= (ord-cmp ord 1 1) 0))
    (is (> (ord-cmp ord 2 1) 0))))

(deftest test-ord-max
  (testing "testing ord-max"
    (let [ord (ord-by)]
      (is (= 6 (ord-max ord 5 6)))
      (is (= 6 (ord-max ord 6)))
      (is (nil? (ord-max ord)))
      (is (= 6 (ord-max ord 1 2 3 4 5 6)))
      ))
  (testing "testing ord-max*"
    (let [ord (ord-by :str)]
      (is (= 2 (ord-max* ord '(1 2 11 12 13))))
      (is (thrown? NullPointerException (ord-max* ord '()))))))


(deftest test-ord-min
  (testing "testing ord-min"
    (let [ord (ord-by)]
      (is (= 5 (ord-min ord 5 6)))
      (is (= 6 (ord-min ord 6)))
      (is (nil? (ord-min ord)))
      (is (= 1 (ord-min ord 1 2 3 4 5 6)))
      ))
  (testing "testing ord-min*"
    (let [ord (ord-by :str)]
      (is (= 1 (ord-min* ord '(1 2 11 12 13))))
      (is (= 0 (ord-min* ord '(9 8 2 3 5 0 7))))
      (is (thrown? NullPointerException (ord-min* ord '()))))))

(deftest test-sort-ordered?
  (let [ord1 (ord-by)
        ord2 (-> (ord-by) (ord-reverse))
        ls (shuffle (take 10 (iterate inc 1)))]
    (is (false? (ordered? ord1 ls)))
    (is (= '(1 2 3 4 5 6 7 8 9 10) (ord-sort ord1 ls)))
    (is (not= ls (ord-sort ord1 ls)))
    (is (ordered? ord1 (ord-sort ord1 ls)))
    (is (strictly-ordered? ord1 (ord-sort ord1 ls)))
    (is (false? (ordered? ord2 (ord-sort ord1 ls))))
    (is (false? (ordered? ord2 ls)))
    (is (false? (ordered? ord1 ls)))
    (is (= (reverse '(1 2 3 4 5 6 7 8 9 10))  (ord-sort ord2 ls)))
    (is (false? (strictly-ordered? ord1 '(1 2 2 3 4 5 6 7 8 9))))
    (is (ordered? ord1 '(1 2 2 3 4 5 6 7 8 9)))
    (is (strictly-ordered? ord1 '(1 2 3 4 5 6 7 8 9)))
    ))

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




