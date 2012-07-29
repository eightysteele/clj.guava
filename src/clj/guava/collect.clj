(ns clj.guava.collect
  (:refer-clojure :exclude [sort max min reverse])
  (:import [com.google.common.base Function]
           [com.google.common.collect ImmutableMultiset]
           [com.google.common.collect Ordering])
  (:use [clj.guava.base :only [var-ns check-not-nil]]))

;;Ordering creation and manipulation
(defn order-by
  "Returns a ordering by type,valid type includes:
       :nat     the natural ordering on Comparable types.
       :str     compares Objects by the lexicographical ordering of
                their string representations, as returned by toString().
       :arb     returns an arbitrary ordering over all objects, for
                which compare(a, b) == 0 implies a == b (identity
                equality). There is no meaning whatsoever to the order
                imposed, but it is constant for the life of the VM.

       comparator   returns a ordering using a java.util.Comparator instance for comparing.

   default type is natural.
  "
  {:tag Ordering :added "0.1"}
  ([]
     (order-by :nat))
  ([type]
     (condp = type
       :nat (Ordering/natural)
       :str    (Ordering/usingToString)
       :arb (Ordering/arbitrary)
       (Ordering/from ^java.util.Comparator type))))

(defn ordering
  "Returns an ordering based on pred"
  {:tag Ordering :added "0.1"}
  [pred]
  (Ordering/from ^java.util.Comparator (comparator pred)))

(defn explicit
  "Returns an ordering that compares objects according to the order in which they are given in the given sequence or varadic arguments."
  {:tag Ordering :added "0.1"}
  ([col]
     (Ordering/explicit (vec col)))
  ([first & others]
     (Ordering/explicit first (into-array Object others))))

(defn reverse
  "Returns the reverse ordering."
  {:tag Ordering :added "0.1"}
  [^Ordering ord]
  (.reverse ord))

(defn nilsfirst
  "Returns an Ordering that orders nulls before non-null elements, and otherwise behaves the same as the original Ordering. "
  {:tag Ordering :added "0.1"}
  [^Ordering ord]
  (.nullsFirst ord))

(defn nilslast
  "Returns an Ordering that orders nulls after non-null elements, and otherwise behaves the same as the original Ordering. "
  {:tag Ordering :added "0.1"}
  [^Ordering ord]
  (.nullsLast ord))

(defn compound
  "Returns an ordering which first uses the ordering this, but which in the event of a \"tie\",
   then delegates to secondaryComparator. For example, to sort a bug list first by status and
   second by priority, you might use byStatus.compound(byPriority). For a compound ordering with
   three or more components, simply chain multiple calls to this method."
  {:tag Ordering :added "0.1"}
  [^Ordering ord ^java.util.Comparator cmp]
  (.compound ord cmp))


;;ordering application
(def ^{:tag Ordering :dynamic true} *ordering* nil)

(defn search
  "Searches sortedList for key using the binary search algorithm,returns the found index,otherwise returns a negative number."
  {:added "0.1"}
  ([col key]
     (check-not-nil *ordering* "*ordering* is not bound")
     (search *ordering* col key))
  ([^Ordering ord col key]
     (.binarySearch ord (list* col) key)))

(defn cmp
  "Compares its two arguments for the ordering. Returns a negative integer, zero, or a positive integer as the first argument is less than, equal to, or greater than the second."
  {:added "0.1"}
  ([left right]
     (check-not-nil *ordering* "*ordering* is not bound")
     (cmp *ordering* left right))
  ([^Ordering ord left right]
     (.compare ord left right)))

(defn greatest
  " Returns the k greatest elements sequence of the given iterable according to the ordering, in order from greatest to least."
  {:tag clojure.lang.ISeq :added "0.1"}
  ([col k]
     (check-not-nil *ordering* "*ordering* is not bound")
     (greatest *ordering* col k))
  ([^Ordering ord col k]
     (seq (.greatestOf ord (seq col) k))))

(defn least
  "Returns the k least elements of the given iterable according to the ordering, in order from least to greatest."
  {:tag clojure.lang.ISeq :added "0.1"}
  ([col k]
     (check-not-nil *ordering* "*ordering* is not bound")
     (least *ordering* col k))
  ([^Ordering ord col k]
     (.leastOf ord (seq col) k)))

(defn max
  "Returns the greatest of the specified values according to the ordering."
  {:added "0.1"}
  ([^Ordering ord] nil)
  ([^Ordering ord x] x)
  ([^Ordering ord x y] (.max ord x y))
  ([^Ordering ord x y z & others]
     (.max ord x y z (into-array Object others))))

(defn max*
  "Returns the least of the specified values according to the ordering."
  {:added "0.1"}
  ([col]
     (check-not-nil *ordering* "*ordering* is not bound")
     (max* *ordering* col))
  ([^Ordering ord col]
     (.max ord ^Iterable (seq col))))

(defn min
  "Returns the greatest of the specified values according to the ordering."
  {:added "0.1"}
  ([^Ordering ord] nil)
  ([^Ordering ord x] x)
  ([^Ordering ord x y] (.min ord x y))
  ([^Ordering ord x y z & others]
     (.min ord x y z (into-array Object others))))

(defn min*
  "Returns the least of the specified values according to the ordering."
  {:added "0.1"}
  ([col]
     (check-not-nil *ordering* "*ordering* is not bound")
     (min* *ordering* col))
  ([^Ordering ord col]
     (.min ord ^Iterable (seq col))))

(defn sort
  "Returns a lazy sorted sequence of the items in coll according to the ordering."
  ([col]
     (check-not-nil *ordering* "*ordering* is not bound")
     (sort *ordering* col))
  ([^Ordering ord col]
     (lazy-seq (.sortedCopy ord (seq col)))))

(defn ordered?
  "Returns true if each element in iterable after the first is greater than or equal to the element that preceded it, according to the ordering."
  {:tag boolean :added "0.1"}
  ([col]
     (check-not-nil *ordering* "*ordering* is not bound")
     (ordered? *ordering* col))
  ([^Ordering ord col]
     (.isOrdered ord (seq col))))

(defn strictly-ordered?
  "Returns true if each element in iterable after the first is strictly greater than the element that preceded it, according to the ordering."
  {:tag boolean :added "0.1"}
  ([col]
     (check-not-nil *ordering* "*ordering* is not bound")
     (strictly-ordered? *ordering* col))
  ([^Ordering ord col]
     (.isStrictlyOrdered ord (seq col))))

(defmacro with-ordering
  "Bind *ordering* to the given ordering and evalute body with this binding."
  {:added "0.1"}
  [^Ordering ord & body]
  `(binding [*ordering* ~ord]
     ~@body))

;;other utilities
(def ^:private boolean-some
  (comp boolean some))

(defmulti include?
  "Returns true if key is present in the given collection, otherwise
  returns false or nil."
  {:added "0.1"}
  (fn [coll key]
    (type coll)))

(prefer-method include? clojure.lang.IPersistentVector java.util.Collection)
(prefer-method include? clojure.lang.IPersistentList java.util.Collection)
(prefer-method include? clojure.lang.IPersistentSet java.util.Collection)
(prefer-method include? clojure.lang.IPersistentMap java.util.Map)

(defmethod include? clojure.lang.IPersistentVector
  [coll key]
  (boolean-some #(= % key) coll))

(defmethod include? clojure.lang.IPersistentList
  [coll key]
  (boolean-some #(= % key) coll))

(defmethod include? clojure.lang.IPersistentSet
  [coll key]
  (boolean-some #(= % key) coll))

(defmethod include? clojure.lang.IPersistentMap
  [coll key]
  (contains? coll key))

(defmethod include? java.util.Collection
  [coll key]
  (boolean-some #(= % key) (seq coll)))

(defmethod include? java.util.Map
  [coll key]
  (.containsKey ^java.util.Map coll key))

(defmethod include? :default
  [coll key]
  (when (seq coll)
    (or (= key (first coll)) (recur (next coll) key))))
