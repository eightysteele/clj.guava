(ns clj.guava.test.primitive
  (:require [clj.guava.primitive :refer :all ])
  (:require [clojure.test :refer :all ]))

(deftest test-primitive-bytes
  (testing "length in bytes of primitve types"
    (is (= byte-bytes 1) "one byte has one byte")
    (is (= char-bytes 2) "one char has two bytes")
    (is (= int-bytes 4) "one int has four bytes")
    (is (= long-bytes 8) "one long has eight bytes")
    (is (= float-bytes 4) "one float has four bytes")
    (is (= double-bytes 8) "one double has eight bytes")
    ))

(deftest test-finite-double
  (testing "test is-finite of double"
    (is (false? (finite-double? Double/NaN)) "Nan is NOT finite")
    (is (false? (finite-double? (/ 1.0 0.0))) "Infinit is NOT finite")
    (is (true? (finite-double? 1.0)) "1.0 is finite")
    (is (true? (finite-double? Double/MAX_VALUE)) "1.0 is finite")
    (is (true? (finite-double? Double/MIN_VALUE)) "1.0 is finite")
    ))

(deftest test-finite-float
  (testing "test is-finite of float"
    (is (false? (finite-float? Float/NaN)) "Nan is NOT finite")
    (is (false? (finite-float? (/ (float 1.0) (float 0.0)))) "Infinit is NOT finite")
    (is (true? (finite-float? (float 1.0))) "1.0 is finite")
    (is (true? (finite-float? Float/MAX_VALUE)) "1.0 is finite")
    (is (true? (finite-float? Float/MIN_VALUE)) "1.0 is finite")
    ))

(deftest char-bytes-transform
  (testing "transforming between char and byte array"
    (for [k (range 0 256)]
      (is (= (char k) (char-from-bytes (char-to-bytes (char k))))))
    (for [k (range -2560 2560)]
      (is (= (int k) (int-from-bytes (int-to-bytes (int k))))))
    (for [k (range -256000 256000)]
      (is (= k (long-from-bytes (long-to-bytes k)))))
    (for [k (range -256000 256000)]
      (is (= (short k) (short-from-bytes (short-to-bytes (short k))))))

    ))
