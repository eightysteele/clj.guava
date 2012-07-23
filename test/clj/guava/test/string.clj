(ns clj.guava.test.string
  (:use [clj.guava.string])
  (:import [com.google.common.base Joiner Splitter CharMatcher Charsets Strings])
  (:use [clojure.test]))

(deftest test-empty-to-nil-and-reverse
  (testing "Test empty->nil and nil->empty")
  (is (= "hello" (empty->nil "hello")))
  (is (= "hello" (nil->empty "hello")))
  (is (nil? (empty->nil nil)))
  (is (nil? (empty->nil "")))
  (is (= " " (empty->nil " ")))
  (is (= "" (nil->empty nil))))

(deftest test-prefix-suffix
  (is (= "hello " (prefix "hello dennis" "hello world")))
  (is (= "" (prefix "hello dennis" "world")))
  (is (= "" (suffix "hello dennis" "world")))
  (is (= " world" (suffix "my world" "hello world"))))

(deftest test-padl-padr
  (is (= "007" (padl "7" 3 \0)))
  (is (= "007" (padl "7" 3 "0")))
  (is (= "2010" (padl "2010" 3 "0")))
  (is (= "2010" (padr "2010" 3 "!")))
  (is (= "4.000" (padr "4." 5 "0")))
  (is (= "4.000" (padr "4." 5 \0))))

(deftest test-repeats
  (is (= "heyheyhey" (repeats "hey" 3)))
  (is (= "" (repeats "" 3))))

(deftest test-nil-or-empty?
  (is (nil-or-empty? nil))
  (is (nil-or-empty? ""))
  (is (not (nil-or-empty? " "))))

(deftest test-join
  (let [jn1 (joiner "," :skip-nils true)
        jn2 (joiner "," :when-nil "repl")
        sb (StringBuilder. "prefix,")]
    (is (= "1,2,3" (jn1 [1 2 3 nil nil])))
    (is (= "1,2,3,repl,repl" (jn2 [1 2 3 nil nil])))
    (is (= "a,b,c" (jn1 ["a" "b" "c"])))
    (is (= "prefix,1,2,3" (str (jn1 sb [1 2 3 nil nil]))))))

(deftest test-starts-ends
  (is (starts? "hello" "he"))
  (is (ends? "hello" "llo"))
  (is (not (starts? "hello" "llo")))
  (is (starts? "12345" "1"))
  (is (not (ends? "hello world" "hello"))))

(deftest test-intern-str
  (is (identical? "hello" (intern-str "hello")))
  (is (not (intern-str nil))))

(deftest test-bytes-seq
  (is (= '(104 101 108 108 111) (bytes-seq "hello")))
  (is (nil? (bytes-seq nil))))

(deftest test-splitter
  (is (= '("foo" "bar" "qux") ((splitter "," :omit-emptys true :trims true) "foo,bar,,   qux"))))

(deftest test-any-of
  (is (instance? CharMatcher (cm-any "abcde" true))))
