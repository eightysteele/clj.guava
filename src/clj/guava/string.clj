(ns ^{ :doc "Wrapper for guava string classes and methods"
      :author "dennis zhuang<killme2008@gmail.com>"}
  clj.guava.string
  (:import [com.google.common.base Joiner Splitter CharMatcher Charsets Strings]))

(definline empty->nil
  "Returns the given string if it is nonempty; nil otherwise."
  { :tag String :added "0.1" :static true}
  [^String string]
  `(Strings/emptyToNull ~string))

(definline nil->empty
  "Returns the given string if it is non-null; the empty string otherwise.nil otherwise."
  { :tag String :added "0.1" :static true}
  [^String string]
  `(Strings/nullToEmpty ~string))

(definline prefix
  "Returns the longest string prefix such that a.toString().startsWith(prefix) && b.toString().startsWith(prefix), taking care not to split surrogate pairs. If a and b have no common prefix, returns the empty string."
  {:tag String :added "0.1"}
  [^CharSequence x ^CharSequence y]
  `(Strings/commonPrefix ~x ~y))

(definline suffix
  "Returns the longest string suffix such that a.toString().endsWith(suffix) && b.toString().endsWith(suffix), taking care not to split surrogate pairs. If a and b have no common suffix, returns the empty string."
  {:tag String :added "0.1"}
  [^CharSequence x ^CharSequence y]
  `(Strings/commonSuffix ~x ~y))

(defn padl
  "Returns a string, of length at least minLength, consisting of string appended with as many copies of padChar as are necessary to reach that length.For example:
 (padl \"7\" 3 \0)  returns \"007\"
 (padl \"2010\" 3 \"0\"  returns \"2010\"
"
  {:tag String :added "0.1"}
  [^String string min pad]
  (if (instance? CharSequence pad)
    (let [ch (.charAt ^CharSequence pad 0)]
      (Strings/padStart string min ch))
    (Strings/padStart string min pad)))

(defn padr
  "Returns a string, of length at least minLength, consisting of string appended with as many copies of padChar as are necessary to reach that length.For example:
 (padr \"4.\" 5 \0)  returns \"4.000\"
 (padr \"2010\" 3 \"!\"  returns \"2010\"
"
  {:tag String :added "0.1"}
  [^String string min pad]
  (if (instance? CharSequence pad)
    (let [ch (.charAt ^CharSequence pad 0)]
      (Strings/padEnd string min ch))
    (Strings/padEnd string min pad)))


(definline repeats
  "Returns a string consisting of a specific number of concatenated copies of an input string.For example:
   (repeats \"hey\" 3) returns \"heyheyhey\"
  "
  {:tag String :added "0.1"}
  [^String string count]
  `(Strings/repeat ~string ~count))

(definline nil-or-empty?
  "Returns true if the given string is null or is the empty string.but (nil-or-empty? \" \") returns false."
  {:tag boolean :added "0.1"}
  [^String string]
  `(Strings/isNullOrEmpty ~string))

(declare join split)

(defn ^{:static true :private true :tag Joiner} create-joiner [sep opt-map]
  (let [{:keys [skip-nils when-nil kv]} opt-map
        ^Joiner jn (Joiner/on (str sep))
        ^Joiner jn (if skip-nils
                     (.skipNulls jn)
                     jn)
        ^Joiner jn (if when-nil
                     (.useForNull jn when-nil)
                     jn)]
    jn))

(defn joiner
  "Returns a partial join function using a joiner which automatically places separator between consecutive elements.Valid options include:

     :skip-nils   true or false,whether skipping over any provided null elements;
     :when-nil    a string text,whether substituting :when-nil text for any provided null elements;

   For example:
     (let [jn (joiner \",\" :skip-nils true)]
        (jn [1 2 3 nil]))
      =>  \"1,2,3\"
    
    (let [jn (joiner \",\" :skip-nils true)
          sb (StringBuilder. \"Prefix\")]
        (jn sb [1 2 3 nil])
        (str sb))
      =>  \"prefix,1,2,3\"

    (let [jn (joiner \",\" :skip-nils true)
          sb (StringBuilder. \"prefix,\")]
        (jn sb 1 2 3 nil)
        (str sb))
      =>  \"prefix,1,2,3\"

   "
  {:added "0.1"}
  [sep & opts]
  (let [m (apply hash-map opts)
        jn (create-joiner sep m)]
    (partial join jn)))

(defn join
  "Use the joiner returns by joiner function to join parts,returns a string or append it to an Appendable such as StringBuilder.
   For example:
     (let [jn (joiner \",\" :skip-nils true)]
        (join jn [1 2 3 nil]))
      =>  \"1,2,3\"
    
    (let [jn (joiner \",\" :skip-nils true)
          sb (StringBuilder. \"Prefix\")]
        (join jn sb [1 2 3 nil])
        (str sb))
      =>  \"prefix,1,2,3\"

    (let [jn (joiner \",\" :skip-nils true)
          sb (StringBuilder. \"prefix,\")]
        (join jn sb 1 2 3 nil)
        (str sb))
      =>  \"prefix,1,2,3\"

"
  ([^Joiner jn ^Appendable appendable col]
     (.appendTo jn appendable ^Iterable (seq col)))
  ([^Joiner jn ^Appendable appendable first & others]
     (.appendTo jn appendable (into-array Object (cons first others))))
  ([^Joiner jn col]
     (.join jn ^Iterable (seq col))))

(defn ^{:private true :tag Splitter} create-splitter
  [sep opts-map]
  (let [{:keys [omit-emptys trims limit]} opts-map
        ^Splitter sp (condp instance? sep
                       String (Splitter/on ^String sep)
                       java.util.regex.Pattern (Splitter/on ^java.util.regex.Pattern sep)
                       Character (Splitter/on (str sep))
                       CharMatcher (Splitter/on ^CharMatcher sep)
                       Long (Splitter/fixedLength ^long sep)
                       (Splitter/on (str sep)))
        ^Splitter sp (if omit-emptys
                       (.omitEmptyStrings sp)
                       sp)
        ^Splitter sp (if limit
                       (.limit sp limit)
                       sp)
        ^Splitter sp (cond
                      (true? trims) (.trimResults sp)
                      (instance? CharMatcher trims) (.trimResults sp trims)
                      :else
                      sp)]
    sp))

(defn splitter
  "Returns a partial function that uses a splitter which uses a given separator(string/pattern/character/CharMatcher) to split string.Valid options include:
     :omit-emptys   true or false,whether to automatically omit empty strings from the results;
     :trims         true,false or CharMatcher,whether to automatically remove leading and trailing
                    whitespace from each returned substring,when it is a CharMatcher,removes all
                    leading or trailing characters matching the given CharMatcher from each returned substring;
     :limit         an integer number,stops splitting after it reaches the limit.

   For example:
    ((splitter \",\" :trims true :omit-emptys true) \"foo,bar,,   qux\")
    =>   (\"foo\" \"bar\" \"qux\")

  "
  {:added "0.1"}
  [sep & opts]
  (let [sp (create-splitter sep (apply hash-map opts))]
    (partial split sp)))

(defn split
  "Split a char sequence with a spliter"
  {:tag clojure.lang.ISeq :added "0.1"}
  [^Splitter sp ^CharSequence cs]
  (lazy-seq (.split sp cs)))

(defn starts?
  "Tests if a string starts with another string,the same with String.startsWith(str,[toffset])"
  {:tag boolean :added "0.1"}
  ([^String x ^String y]
     (starts? x y 0))
  ([^String x ^String y n]
     (.startsWith x y n)))

(definline ends?
  "Tests if a string ends with another string,the same with String.endsWith(str)."
  {:tag boolean :added "0.1"}
  [^String x ^String y]
  `(.endsWith ~x ~y))

(definline intern-str
  "Intern a string using String.intern."
  {:tag String :added "0.1"}
  [^String x]
  `(when ~x
     (.intern ~x)))

(defn bytes-seq
  "Returns a lazy bytes sequence of a string using String.getBytes"
  {:tag clojure.lang.ISeq
   :added "0.1"
   :static true}
  ([^String string]
     (when string
       (lazy-seq (.getBytes string))))
  ([^String string charset]
     (when string
       (lazy-seq (.getBytes string (str charset))))))


(defn- guava-char-macher-name [^String name]
  (.replace (.toUpperCase name) "_" "-"))

;; define clojure constants for every CharMacher constants
(doseq [^java.lang.reflect.Field f (seq (.getFields CharMatcher))]
  (let [name (.getName f)
        doc (format "CharMatcher/%s" name)
        new-name (guava-char-macher-name name)]
    (eval
     `(def ~(symbol new-name) (. CharMatcher ~(symbol name))))))

(def ALL-MATCHERS
  (doall (map #(.get ^java.lang.reflect.Field % nil) (seq (.getFields CharMatcher)))))


(defmacro ^:private defmatcher
  [name new-name doc added]
  `(defn ~new-name
     ~doc
     {:tag CharMatcher :added ~added :arglists '([~'x] [~'x ~'precomputed])}
     ([cs#]
        (~new-name cs# false))
     ([cs# precomputed#]
        (let [cm# (. CharMatcher ~name cs#)]
          (if precomputed#
            (.precomputed ^CharMatcher cm#)
            cm#)))))

;;Define functions to create char macher.
(defmatcher anyOf cm-any "Returns a char matcher that matches any character present in the given character sequence." "0.1")

(defmatcher noneOf cm-none "Returns a char matcher that matches any character not present in the given character sequence." "0.1")

(defmatcher is cm-is "Returns a char matcher that matches only one specified character." "0.1")

(defmatcher isNot cm-is-not "Returns a char matcher that matches any character except the one specified." "0.1")

(defn ma-range
  " Returns a char matcher that matches any character in a given range (both endpoints are inclusive)."
  {:tag CharMatcher :added "0.1"}
  [start end]
  (CharMatcher/inRange start end))

(defn cm-and
  "Returns a matcher that matches any character matched by both this matcher and other."
  {:tag CharMatcher :added "0.1"}
  [^CharMatcher x ^CharMatcher y]
  (.and x y))

(defn cm-or
  "Returns a matcher that matches any character matched by either this matcher or other."
  {:tag CharMatcher :added "0.1"}
  [^CharMatcher x ^CharMatcher y]
  (.or x y))

(defn cm-neg
  "Returns a matcher that matches any character not matched by this matcher."
  {:tag CharMatcher :added "0.1"}
  [^CharMatcher x]
  (.negate x))

;;Define char macher functions for usage.

(definline cm-collapse
  "Returns a string copy of the input character sequence, with each group of consecutive characters that match this matcher replaced by a single replacement character."
  [^CharMatcher cm ^CharSequence s ^Character ch]
  {:tag String :added "0.1"}
  `(.collapseFrom ~cm ~s ~ch))

(definline cm-count
  " Returns the number of matching characters found in a character sequence."
  {:added "0.1"}
  [^CharMatcher cm ^CharSequence s]
  `(.countIn ~cm ~s))

(defn cm-index
  "Returns the index of the first matching character in a character sequence, or -1 if no matching character is present."
  {:added "0.1"}
  ([cm s]
     (cm-index cm s 0))
  ([^CharMatcher cm ^CharSequence s start]
     (.indexIn cm s start)))

(definline cm-last-index
  "Returns the index of the last matching character in a character sequence, or -1 if no matching character is present."
  {:added "0.1"}
  [^CharMatcher cm ^CharSequence s]
  `(.lastIndexIn ~cm ~s))

(defn cm-matches
  "Determines a true or false value for the given character or string. Valid types:
      :any   returns true if a character sequence contains at least one matching character.
      :all   returns true if a character sequence contains only matching characters.
      :none  returns true if a character sequence contains no matching characters."
  {:tag boolean :added "0.1"}
  ([^CharMatcher cm ch]
     (cm-matches cm (str ch) :all))
  ([^CharMatcher cm ^CharSequence s type]
     (condp = type
       :all (.matchesAllOf cm s)
       :any (.matchesAnyOf cm s)
       :none (.matchesNoneOf cm s)
       (throw (IllegalArgumentException. (format "Unknow maches type %s,valid types include :all :any :none." type))))))

(defn cm-replace
  "Returns a string copy of the input character sequence, with each character that matches this matcher replaced by a given replacement character."
  {:added "0.1" :tag String}
  [^CharMatcher cm ^CharSequence s replacement]
  (.replaceFrom cm s (str replacement)))

(defn cm-remove
  "Returns a string containing all non-matching characters of a character sequence, in order."
  {:added "0.1" :tag String}
  [^CharMatcher cm ^CharSequence s]
  (.removeFrom cm s))

(defn cm-retain
  " Returns a string containing all matching characters of a character sequence, in order."
  {:added "0.1" :tag String}
  [^CharMatcher cm ^CharSequence s]
  (.retainFrom cm s))

(defn cm-trim
  "Returns a substring of the input character sequence that omits all characters this matcher matches from the beginning and from the end of the string."
  {:added "0.1" :tag String}
  [^CharMatcher cm ^CharSequence s]
  (.trimFrom cm s))

(defn cm-triml
  "Returns a substring of the input character sequence that omits all characters this matcher matches from the beginning of the string."
  {:added "0.1" :tag String}
  [^CharMatcher cm ^CharSequence s]
  (.trimLeadingFrom cm s))

(defn cm-trimr
  "Returns a substring of the input character sequence that omits all characters this matcher matches from the end of the string."
  {:added "0.1" :tag String}
  [^CharMatcher cm ^CharSequence s]
  (.trimTrailingFrom cm s))











