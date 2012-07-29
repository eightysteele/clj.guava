(ns ^{:doc "Wrapper of guava primitives"
      :author "qiuxiafei <qiuxiafei@gmail.com>"}
  clj.guava.primitive
  (:import [com.google.common.primitives
            Bytes SignedBytes UnsignedBytes
            Ints UnsignedInteger UnsignedInts
            Longs UnsignedLong UnsignedLongs
            Floats Doubles Chars Booleans Shorts]))

; length in bytes
(def byte-bytes 1)

(def char-bytes Chars/BYTES)

(def int-bytes Ints/BYTES)

(def float-bytes Floats/BYTES)

(def double-bytes Doubles/BYTES)

(def long-bytes Longs/BYTES)

; max unsinged primitives
(def max-ulong UnsignedLong/MAX_VALUE)

(def max-uint UnsignedInteger/MAX_VALUE)

; finit float/double 
(defn finit-double? [arg]
  (Doubles/isFinite arg))

(defn finit-float? [arg]
  (Doubles/isFinite arg))

; transform between primitives and byte array
(defn char-from-bytes [bytes]
  (Chars/fromByteArray bytes))

(defn char-to-bytes [c]
  (Chars/toByteArray c))

(defn int-from-bytes [bytes]
  (Ints/fromByteArray bytes))

(defn int-tobytes [n]
  (Ints/toByteArray n))

(defn short-from-bytes [bytes]
  (Shorts/fromByteArray bytes))

(defn short-to-bytes [s]
  (Shorts/toByteArray))

(defn long-to-bytes [l]
  (Longs/toByteArray l))

(defn long-from-bytes [bytes]
  (Longs/fromByteArray bytes))

