(ns dram.test.parser
  (:require [dram.parser :as p]
            [clojure.test :refer :all]
            [the.parsatron :refer [run]]))


(defmacro is-error [input parser]
  `(~'is (~'thrown? RuntimeException (run ~parser ~input))))

(defmacro parses-as [input parser output]
  `(~'is (~'= ~output (run ~parser ~input))))


(deftest whitespace-test
  (testing "Optional whitespace parses to nil."
    (parses-as "" (p/optional-whitespace) nil)
    (parses-as "     " (p/optional-whitespace) nil)
    (parses-as "\t" (p/optional-whitespace) nil)
    (parses-as "\n" (p/optional-whitespace) nil)
    (parses-as "\n \t " (p/optional-whitespace) nil))
  (testing "Required whitespace parses to nil."
    (parses-as "     " (p/required-whitespace) nil)
    (parses-as "\t" (p/required-whitespace) nil)
    (parses-as "\n" (p/required-whitespace) nil)
    (parses-as "\n \t " (p/required-whitespace) nil))
  (testing "Required whitespace is actually required."
    (is-error "" (p/required-whitespace))
    (is-error "foo " (p/required-whitespace))))

(deftest integer-test
  (testing "Parsing integers results in Clojure integers."
    (parses-as "1" (p/literal-integer) 1)
    (parses-as "10" (p/literal-integer) 10)
    (parses-as "9234" (p/literal-integer) 9234))
  (testing "Parsing negative integers."
    (parses-as "-1" (p/literal-integer) -1)
    (parses-as "-2945" (p/literal-integer) -2945))
  (testing "Parsing garbage with the integer parser fails."
    (is-error "" (p/literal-integer))
    (is-error " 1" (p/literal-integer))
    (is-error "foo" (p/literal-integer))
    (is-error "-" (p/literal-integer))
    (is-error " -1" (p/literal-integer))
    (is-error "- 1" (p/literal-integer))
    (is-error "-a12" (p/literal-integer))))

(deftest string-test
  (testing "Literal strings of simple characters parse to Clojure strings."
    (parses-as "\"\"" (p/literal-string) "")
    (parses-as "\"foo\"" (p/literal-string) "foo")
    (parses-as "\" bar \"" (p/literal-string) " bar "))
  (testing "Escape sequences are supported in strings."
    (parses-as "\"a\\nb\"" (p/literal-string) "a\nb")
    (parses-as "\"a\\\\b\"" (p/literal-string) "a\\b")
    (parses-as "\"a\\\\nb\"" (p/literal-string) "a\\nb")
    (parses-as "\"a\\\"b\"" (p/literal-string) "a\"b"))
  (testing "Garbage doesn't parse as strings."
    (is-error "foo" (p/literal-string))
    (is-error "\"foo" (p/literal-string))
    (is-error "foo\"" (p/literal-string)))
  (testing "Parses the first bit as a string, so it should succeed (for now)."
    (parses-as "\"fo\"o\"" (p/literal-string) "fo")))

(deftest literal-test
  (testing "Literals can parse integers."
    (parses-as "-42" (p/literal) -42)
    (parses-as "585" (p/literal) 585))
  (testing "Literals can parse strings."
    (parses-as "\"foo\"" (p/literal-string) "foo")))

(deftest variable-test
  (testing "Variables can be simple literals (though I don't know why you'd bother)."
    (parses-as "{{ 42 }}" (p/variable) 42)
    (parses-as "{{ -2 }}" (p/variable) -2)
    (parses-as "{{ \"foo\" }}" (p/variable) "foo"))
  (testing "Variables can handle wonky whitespace."
    (parses-as "{{42}}" (p/variable) 42)
    (parses-as "{{ 42}}" (p/variable) 42)
    (parses-as "{{42 }}" (p/variable) 42)
    (parses-as "{{42       }}" (p/variable) 42)
    (parses-as "{{\n\t\n\t42}}" (p/variable) 42)))
