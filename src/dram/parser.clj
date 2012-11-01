(ns dram.parser
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))


; Utilities -------------------------------------------------------------------
(defparser optional [p]
  (either (attempt p)
          (always nil)))


; Whitespace ------------------------------------------------------------------
(defparser whitespace-char []
  (token #{\space \newline \tab}))

(defparser optional-whitespace []
  (many (whitespace-char))
  (always nil))

(defparser required-whitespace []
  (many1 (whitespace-char))
  (always nil))


; Numbers ---------------------------------------------------------------------
(defparser literal-integer-pos []
  (let->> [digits (many1 (digit))]
    (always (Integer/parseInt (apply str digits)))))

(defparser literal-integer-neg []
  (char \-)
  (let->> [n (literal-integer-pos)]
    (always (- n))))

(defparser literal-integer []
  (either (attempt (literal-integer-neg))
          (literal-integer-pos)))


; Strings ---------------------------------------------------------------------
(defparser literal-string-escape []
  (char \\)
  (let->> [ch (any-char)]
    (always (case ch
              \\ \\
              \n \newline
              \" \"
              ch))))

(defparser literal-string-char []
  (either (attempt (literal-string-escape))
          (token (complement #{\"}))))

(defparser literal-string-empty []
  (string "\"\"")
  (always ""))

(defparser literal-string-nonempty []
  (between (char \") (char \")
           (let->> [contents (many1 (literal-string-char))]
             (always (apply str contents)))))

(defparser literal-string []
  (either (attempt (literal-string-empty))
          (attempt (literal-string-nonempty))))


; Literals --------------------------------------------------------------------
(defparser literal []
  (choice (literal-integer)
          (literal-string)))


; Paths -----------------------------------------------------------------------
(defparser path-char []
  (choice (letter)
          (digit)
          (token #{\_ \-})))

(defparser path-segment []
  (let->> [contents (many1 (path-char))]
    (always (apply str contents))))

(defparser path []
  (let->> [seg (path-segment)
           segs (many (>> (char \.) (path-segment)))]
    (always (concat [seg] segs))))


; Context Values --------------------------------------------------------------
(defparser value-filter-arg []
  (choice (literal)
          (path)))

(defparser value-filter-args []
  (char \:)
  (let->> [arg (value-filter-arg)
           args (many (>> (char \,) (value-filter-arg)))]
    (always (concat [arg] args))))

(defparser value-filter []
  (char \|)
  (let->> [filter-path (path)
           filter-args (optional (value-filter-args))]
    (always {:path filter-path
             :args (or filter-args [])})))

(defparser value []
  (let->> [base (choice (literal) (path))
           filters (many (value-filter))]
    (always {:type :value
             :base base
             :filters filters})))


; Variables -------------------------------------------------------------------
(defparser variable-open []
  (string "{{")
  (optional-whitespace)
  (always nil))

(defparser variable-close []
  (optional-whitespace)
  (string "}}")
  (always nil))

(defparser variable []
  (between (variable-open) (variable-close)
           (choice (literal))))


; Tags ------------------------------------------------------------------------
(defparser tag-open []
  (string "{%")
  (optional-whitespace)
  (always nil))

(defparser tag-close []
  (optional-whitespace)
  (string "%}")
  (always nil))

(defparser tag-guts []
  (always nil))

(defparser tag []
  (between (tag-open) (tag-close)
           (tag-guts)))


; Extends ---------------------------------------------------------------------
(defparser tag-extends []
  (between (tag-open) (tag-close)
           (let->> [_ (string "extends")
                    _ (required-whitespace)
                    path (literal-string-nonempty)]
             (always {:type :extends
                      :path path}))))


; Block -----------------------------------------------------------------------
(declare template-chunk)

(defparser tag-block-name-char []
  (choice (letter)
          (digit)
          (token #{\- \_})))

(defparser tag-block-name []
  (let->> [fch (letter)
           chs (many (tag-block-name-char))]
    (always (apply str (concat [fch] chs)))))

(defparser tag-block-open []
  (between (tag-open) (tag-close)
           (let->> [_ (string "block")
                    _ (required-whitespace)
                    name (tag-block-name)]
             (always {:name name}))))

(defparser tag-block-close []
  (between (tag-open) (tag-close)
           (let->> [_ (string "endblock")]
             (always nil))))

(defparser tag-block-contents []
  (many (template-chunk)))

(defparser tag-block []
  (let->> [{:keys [name]} (tag-block-open)
           contents (tag-block-contents)
           close (tag-block-close)]
    (always {:type :block
             :name name
             :contents contents})))


; Raw Text --------------------------------------------------------------------
(defparser raw-text-char []
  (let->> [ch (any-char)
           nch (lookahead (optional (token #{\{ \%})))]
    (if (and (= ch \{) nch)
      (never)
      (always ch))))

(defparser raw-text []
  (let->> [contents (many1 (attempt (raw-text-char)))]
    (always (apply str contents))))


; High-Level ------------------------------------------------------------------
(defparser template-chunk []
  (choice (attempt (variable))
          (attempt (raw-text))))

(defparser template-base []
  (let->> [contents (many (choice (attempt (tag-block))
                                  (attempt (template-chunk))))
           _ (eof)]
    (always {:type :base :contents contents})))

(defparser template-child []
  (let->> [{:keys [path]} (between (optional-whitespace) (optional-whitespace)
                                   (tag-extends))
           blocks (many (between (optional-whitespace) (optional-whitespace)
                                 (tag-block)))
           _ (eof)]
    (always {:type :child :extends path
             :blocks (into {} (map (juxt :name :contents) blocks))})))

(defparser template []
  (choice (template-base)
          (template-child)))


; Main ------------------------------------------------------------------------
(defn parse
  "Parse the given string into a Dram AST."
  [string]
  (run (>> (literal-string)) string))


(comment
  (parse "\"foo\"")
  )
