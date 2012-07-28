(ns clj.guava.base
  (:import [com.google.common.base Objects Objects$ToStringHelper Throwables Preconditions]
           [com.google.common.base Function]
           [com.google.common.collect Ordering]))

(defn  hash-code
  "Generates a hash code for multiple values."
  {:tag int :static true :added "0.1"}
  [ x & rest]
  (Objects/hashCode (into-array Object (cons x rest))))


(def ^{:tag Objects$ToStringHelper :dynamic true} *str-helper* nil)

(defn omit-nils
  "Omit nil values in addv,it must be in with-str-helper"
  {:added "0.1"}
  []
  (if *str-helper*
    (.omitNullValues *str-helper*)
    (throw (NullPointerException. "omit-nils not in with-str-helper body"))))

(defn addv
  "Adds a name/value pair to the formatted output in name=value format,it must be in with-str-helper"
  { :added "0.1" }
  ([x]
     (if *str-helper*
       (.addValue *str-helper* x)
       (throw (NullPointerException. "addv is not in with-str-helper body"))))
  ([k x]
     (if *str-helper*
       (.add *str-helper* (name k) x)
       (throw (NullPointerException. "addv is not in with-str-helper body")))))

(defmacro with-str-helper
  "Returns a string in a format"
  {:tag String :added "0.1"}
  [ obj & body ]
  (let [sym (second body)]
    (if (and (= (first body) :as) (symbol? sym))
      `(let [~sym ~obj]
         (binding [*str-helper* (Objects/toStringHelper ~sym)]
           ~@body
           (.toString *str-helper*)))
      `(binding [*str-helper* (Objects/toStringHelper ~obj)]
         ~@body
         (.toString *str-helper*)))))

;;Preconditions

(defn check-valid-options
  "Throws an exception if the given option map contains keys not listed
  as valid, else returns nil.
   This function is a private function in clojure.core.
 "
  [options & valid-keys]
  {:added "0.1"}
  (when (seq (apply disj (apply hash-set (keys options)) valid-keys))
    (throw
     (IllegalArgumentException.
      (apply str "Only these options are valid: "
             (first valid-keys)
             (map #(str ", " %) (rest valid-keys)))))))

(defn check-arg
  " Ensures the truth of an expression involving one or more parameters to the calling method."
  {:added "0.1"}
  ([exp]
     (Preconditions/checkArgument exp))
  ([exp msg]
     (Preconditions/checkArgument exp msg))
  ([exp fmt & objs]
     (Preconditions/checkArgument exp fmt (into-array Object objs))))

(defmacro check-args
  "Assert arguments are valid, a pair has  an argument test and a failure message ,for example:
           (defn test [ bindings & body]
               (check-args (vector? bindings) \"bindings must be a vector\"
                                   (even? (count bindings)) \"an even number of forms in binding vector\")
                ......)

  This macro is a private macro in clojure.core.
 "
  {:added "0.1"}
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

(defn check-not-nil
  "Ensures that an object reference passed as a parameter to the calling method is not null,else throw NPE."
  {:added "0.1"}
  ([obj]
     (Preconditions/checkNotNull obj))
  ([obj msg]
     (Preconditions/checkNotNull obj msg))
  ([obj fmt & args]
     (Preconditions/checkNotNull obj fmt (into-array Object args))))

(defn check-state
  " Ensures the truth of an expression involving the state of the calling instance, but not involving any parameters to the calling method,otherwise throw IllegalStateException."
  {:added "0.1"}
  ([obj]
     (Preconditions/checkState obj))
  ([obj msg]
     (Preconditions/checkState obj msg))
  ([obj fmt & args]
     (Preconditions/checkState obj fmt (into-array Object args))))

(defn check-pos-index
  "Ensures that index specifies a valid position in an array, list string or other sequence of size size. A position index may range from zero to size, inclusive.For example:
   (check-pos-index index size)
   (check-pos index index size \"out of index\")
   (check-pos-index start index size)
 "
  ([idx size]
     (Preconditions/checkPositionIndex idx size)) 
  ([x y z]
     (if (instance? String z)
       (Preconditions/checkPositionIndex x y ^String z)
       (Preconditions/checkPositionIndex x y ^long z))))

(defn check-el-index
  "Ensures that index specifies a valid element in an array, list,string or other sequences of size size. An element index may range from zero, inclusive, to size, exclusive."
  {:added "0.1"}
  ([idx size]
     (Preconditions/checkElementIndex idx size))
  ([idx size desc]
     (Preconditions/checkElementIndex idx size desc)))

;;Throwables
(defn st-str
  "Returns a string containing the result of toString(), followed by the full, recursive stack trace of throwable."
  {:tag String :added "0.1"}
  [^Throwable t]
  (Throwables/getStackTraceAsString t))

(defn root-cause
  " Returns the innermost cause of throwable."
  {:tag Throwable :added "0.1"}
  [^Throwable t]
  (Throwables/getRootCause t))

(defn cause-seq
  "Returns a sequence contains a throwable cause chain"
  {:tag clojure.lang.ISeq :added "0.1"}
  [^Throwable t]
  (seq (Throwables/getCausalChain t)))

