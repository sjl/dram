(ns dram.context)


(defn to-keys
  "Return a vector of possible keys for this string.

  Possible keys for a string include the string itself or a keyword version of
  it.  If the string contains something parseable as an integer, an integer is
  also a possibility.

  Note that by integer I actually mean long because Clojure is stupid at
  comparing the two.  But whatever, you get the idea.

  "
  [s]
  (let [i (try
            (Long. s)
            (catch java.lang.NumberFormatException e nil))
        k (keyword s)]
    (if i
      [i k s]
      [k s])))

(defn try-keys [keys context]
  (when (seq keys)
    (let [[k & ks] keys]
      (if (contains? context k)
        (get context k)
        (recur ks context)))))

(defn lookup [path context]
  (cond
    (empty? path) context
    (nil? context) nil
    :else (let [[head & tail] path
                possible-keys (to-keys head)
                result (try-keys possible-keys context)]
            (recur tail result))))

