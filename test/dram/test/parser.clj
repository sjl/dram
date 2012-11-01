(ns dram.test.parser
  (:require [dram.parser :as p]
            [clojure.test :refer :all]
            [the.parsatron :refer [run]]))


(defmacro is-error [input parser]
  `(~'is (~'thrown? RuntimeException (run ~parser ~input))))

(defmacro parses-as [input parser output]
  `(~'is (~'= ~output (run ~parser ~input))))

(defmacro testing-parser [parser desc & data]
  (let [pairs (partition 2 data)]
    (apply list 'testing desc
           (map (fn [[input output]]
                  `(parses-as ~input ~parser ~output))
                pairs))))

(defmacro testing-parser-errors [parser desc & data]
  (apply list 'testing desc
         (map (fn [input]
                `(is-error ~input ~parser))
              data)))


(deftest optional-test
  (testing-parser
    (p/optional (p/literal-integer))
    "The optional parser makes another parser optional!"

    "42" 42
    "a"  nil))

(deftest whitespace-test
  (testing-parser
    (p/optional-whitespace) "Optional whitespace parses to nil."

    ""       nil
    "     "  nil
    "\t"     nil
    "\n"     nil
    "\n \t " nil)

  (testing-parser
    (p/required-whitespace) "Required whitespace parses to nil."

    "     "  nil
    "\t"     nil
    "\n"     nil
    "\n \t " nil)

  (testing-parser-errors
    (p/required-whitespace) "Required whitespace is actually required."

    ""
    "foo"))

(deftest integer-test
  (testing-parser
    (p/literal-integer) "Parsing integers results in Clojure integers."

    "1"    1
    "10"   10
    "9234" 9234)

  (testing-parser
    (p/literal-integer) "Parsing negative integers results in integers too."

    "-1"    -1
    "-2945" -2945)

  (testing-parser-errors
    (p/literal-integer) "Parsing garbage with the integer parser fails."

    ""
    " 1"
    "foo"
    "-"
    " -1"
    "- 1"
    "-a12"))

(deftest string-test
  (testing-parser
    (p/literal-string)
    "Literal strings of simple characters parse to Clojure strings."

    "\"\""      ""
    "\"foo\""   "foo"
    "\" bar \"" " bar ")

  (testing-parser
    (p/literal-string)
    "Escape sequences are supported in strings."

    "\"a\\nb\""   "a\nb"
    "\"a\\\\b\""  "a\\b"
    "\"a\\\\nb\"" "a\\nb"
    "\"a\\\"b\""  "a\"b")

  (testing-parser-errors
    (p/literal-string)
    "Garbage doesn't parse as strings."

    "foo"
    "\"foo"
    "foo\"")

  (testing-parser
    (p/literal-string)
    "Parses the first bit as a string, so it should succeed (for now)."

    "\"fo\"o\"" "fo"))

(deftest literal-test
  (testing-parser
    (p/literal) "Literals can parse integers."

    "-42" -42
    "585" 585)

  (testing-parser
    (p/literal) "Literals can parse strings."

    "\"foo\"" "foo"))

(deftest variable-test
  (testing-parser
    (p/variable) "Variables can be simple literals."

    "{{ 42 }}"      42
    "{{ -2 }}"      -2
    "{{ \"foo\" }}" "foo")

  (testing-parser
    (p/variable) "Variables can handle wonky whitespace."

    "{{42}}"         42
    "{{ 42}}"        42
    "{{42 }}"        42
    "{{42  }}"       42
    "{{\n\t\n\t42}}" 42))

(deftest extends-test
  (testing-parser
    (p/tag-extends)
    "{% extends ... %} parses to its own custom AST element."

    "{% extends \"p\" %}"        {:type :extends :path "p"}
    "{% extends \"foo/bar\" %}"  {:type :extends :path "foo/bar"})

  (testing-parser-errors
    (p/tag-extends)
    "{% extends ... %} requires a non-empty argument."

    "{% extends \"\" %}"
    "{% extends %}")

  (testing-parser-errors
    (p/tag-extends)
    "{% extends ... %} doesn't accept garbage."

    "{% extends foo %}"
    "{% extends \"foo\" foo %}"
    "{% extends foo \"foo\" %}"
    "{% extends foo\"foo\" %}"
    "{% extends 43 %}"
    "{% extends foo/bar %}"
    "{% extends the quick brown fox %}"))

(deftest block-test
  (testing-parser
    (p/tag-block-open)
    "{% block ... %} parses to an intermediate AST element."

    "{% block cats %}"            {:name "cats"}
    "{% block boots-and-cats %}"  {:name "boots-and-cats"}
    "{% block hello-world_585 %}" {:name "hello-world_585"}
    "{% block a %}"               {:name "a"}
    "{% block a_ %}"              {:name "a_"})

  (testing-parser-errors
    (p/tag-block-open)
    "{% block ... %} requires valid block names."

    "{% block 1 %}"
    "{% block -1 %}"
    "{% block -foo %}"
    "{% block __foo %}"
    "{% block 12dogs %}"
    "{% block c&ats %}"
    "{% block boots and cats %}"
    "{% block \"rochester-made\" %}"
    "{% block dogs* %}"
    "{% block dogs% %}"
    "{% block dogs} %}")

  (testing-parser
    (p/tag-block-open)
    "{% block ... %} allows wonky whitespace."

    "{%block foo%}"           {:name "foo"}
    "{%   block foo%}"        {:name "foo"}
    "{%block      foo     %}" {:name "foo"}
    "{%\n\nblock\tfoo\n%}"    {:name "foo"})

  (testing-parser-errors
    (p/tag-block-open)
    "{% block ... %} REQUIRES whitespace between block and the name."

    "{% blockfoo %}")

  (testing-parser
    (p/tag-block-close)
    "{% endblock %} parses to nil and allows weird whitespace."

    "{% endblock %}"   nil
    "{%\nendblock\t%}" nil
    "{%endblock %}"    nil
    "{% endblock%}"    nil)

  (testing-parser-errors
    (p/tag-block-close)
    "{% endblock %} does NOT take a block name (for now)."

    "{% endblock foo %}")

  (testing-parser
    (p/tag-block)
    "Empty blocks are totally fine."

    "{% block foo %}{% endblock %}" {:type :block :name "foo" :contents []})

  (testing-parser
    (p/tag-block)
    "Blocks can contain anything except other blocks."

    "{% block foo %}hi{% endblock %}"
    {:type :block :name "foo" :contents ["hi"]}

    "{% block foo %}hi {{ 1 }} five{% endblock %}"
    {:type :block :name "foo" :contents ["hi " 1 " five"]}))

(deftest raw-text-test
  (testing-parser
    (p/raw-text) "Raw text parses to a Clojure string."

    "Hello"             "Hello"
    "hello there world" "hello there world"
    "  { foo } is okay" "  { foo } is okay"
    "so is { % foo % }" "so is { % foo % }")

  (testing-parser
    (p/raw-text) "Reserved characters do not parse as raw text."

    "Hello{{ world }}"       "Hello"
    "Hello{% block world %}" "Hello")

  (testing-parser-errors
    (p/raw-text) "Raw text is not zero-length."

    "{{ world }}"
    ""))

(deftest path-test
  (testing-parser
    (p/path) "A path parses to a seq of strings."

    "hello"             ["hello"]
    "hello.world"       ["hello" "world"]
    "users.0"           ["users" "0"]
    "users.0.full-name" ["users" "0" "full-name"]
    "0"                 ["0"]
    "user.full_name"    ["user" "full_name"])

  (testing-parser-errors
    (p/path) "A path can't parse garbage."

    "/foo"
    "..oo"))

(deftest template-chunk-test
  (testing-parser
    (p/template-chunk) "A template chunk can be raw text."

    "Hello"     "Hello"
    "  { foo }" "  { foo }")

  (testing-parser
    (p/template-chunk) "A template chunk can be a variable."

    "{{ 1 }}" 1))

(deftest template-base-test
  (letfn [(bt [contents]
            {:type :base :contents contents})]
    (testing-parser
      (p/template-base)
      "A base template can be made up of raw text, variables, ...."

      ""                        (bt [])
      "Hello"                   (bt ["Hello"])
      "Hello {{ \"Steve\" }}"   (bt ["Hello " "Steve"])
      "Age: {{ 27 }} years old" (bt ["Age: " 27 " years old"]))

    (testing-parser
      (p/template-base)
      "A base template can contain blocks."

      "{% block foo %}{% endblock %}"
      (bt [{:type :block :name "foo" :contents []}])

      "hello {% block username %}{% endblock %}"
      (bt ["hello "
           {:type :block :name "username" :contents []}])

      "foo {% block a %}{% endblock %} bar {{ 42 }}"
      (bt ["foo "
           {:type :block :name "a" :contents []}
           " bar "
           42]))))

(deftest template-child-test
  (letfn [(ct [extends blocks]
            {:type :child :extends extends :blocks blocks})]
    (testing-parser
      (p/template-child)
      "A child template requires an extends tag."

      "{% extends \"a\" %}"     (ct "a" {})
      "  {% extends \"a\" %}"   (ct "a" {})
      "{% extends \"a\" %}\n\n" (ct "a" {}))

    (testing-parser
      (p/template-child)
      "A child template may contain blocks to override."

      "
      {% extends \"a\" %}
      {% block foo %}{% endblock %}
      "
      (ct "a" {"foo" []})

      "
      {% extends \"a\" %}
      {% block foo %}hello world{% endblock %}
      {% block bar %}{{ 10 }}{% endblock %}
      "
      (ct "a" {"foo" ["hello world"]
               "bar" [10]}))))
