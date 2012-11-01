(ns dram.parser
  (:refer-clojure :exclude [char])
  (:use [the.parsatron]))


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



; Main ------------------------------------------------------------------------
(defn parse
  "Parse the given string into a Dram AST."
  [string]
  (run (>> (literal-string)) string))


(comment
  (parse "\"foo\"")
  )
