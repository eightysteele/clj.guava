(ns clj.guava.base
  (:import [com.google.common.base Objects Objects$ToStringHelper Throwables]
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

(defn check-options
  "Throws an exception if the given option map contains keys not listed
  as valid, else returns nil.
   This function is from clojure.core private functions.
 "
  [options & valid-keys]
  {:added "0.1"}
  (when (seq (apply disj (apply hash-set (keys options)) valid-keys))
    (throw
     (IllegalArgumentException.
      (apply str "Only these options are valid: "
             (first valid-keys)
             (map #(str ", " %) (rest valid-keys)))))))

(defmacro check-args
  "Assert arguments are valid, a pair has  an argument test and a failure message ,for example:
           (defn test [ bindings & body]
               (assert-args (vector? bindings) \"bindings must be a vector\"
                                   (even? (count bindings)) \"an even number of forms in binding vector\")
                ......)

  This macro is from clojure.core private macros.
 "
  {:added "0.1"}
  [& pairs]
  `(do (when-not ~(first pairs)
         (throw (IllegalArgumentException.
                 (str (first ~'&form) " requires " ~(second pairs) " in " ~'*ns* ":" (:line (meta ~'&form))))))
       ~(let [more (nnext pairs)]
          (when more
            (list* `assert-args more)))))

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

