(ns clj.guava.primitives.booleans
  clj.guava.primitives.booleans
  (:import [com.google.common.primitives Booleans]))


(defn as-list
  ""
  [& args]
  (Booleans/asList (into-array Boolean/TYPE (vec args))))

(defn compare
  ""
  [a b]
  (Booleans/compare a b))

(defn concat
  "(concat (boolean-array [true false]) (boolean-array [false true]))"
  [& args]
  (Booleans/concat (into-array args))) ; TODO type of boolean array ?

(defn contains
  "(contains (boolean-array [true true]) true)"
  [arr target]
  (Booleans/contains arr target))

(defn ensureCapacity
  "(ensureCapacity (boolean-array [true true]) 2 1)"
  [arr min-len padding]
  (Booleans/ensureCapacity (into-array Boolean/TYPE arr) min-len padding))

(defn hash-code
  "(hash-code true)"
  [arg]
  (Booleans/hashCode arg))

(defn index-of
  "(index-of  [false true] true) or  (index-of  [false true false] (boolean-array [false true]))"
  [arr target]
  (Booleans/indexOf (boolean-array arr) target))

(defn last-index-of
  "(last-index-of [false true]) true)"
  [arr target]
  (Booleans/lastIndexOf (boolean-array arr) target))


(defn join
  ""
  [sep & args]
  (Booleans/join sep (boolean-array args)))

(defn join-array
  "(join-array \"&-\" [true false])"
  [sep arr]
  (Booleans/join sep (boolean-array arr)))

;; TODO comparator??

(defn to-array
  "(to-array '(true false true))"
  [coll]
  (Booleans/toArray coll))
