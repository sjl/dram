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

(defparser literal-string []
  (between (char \") (char \")
           (let->> [contents (many (literal-string-char))]
             (always (apply str contents)))))


; Main ------------------------------------------------------------------------
(defn parse
  "Parse the given string into a Dram AST."
  [string]
  (run (>> (literal-string)) string))


(comment
  (parse "")
  ) 
