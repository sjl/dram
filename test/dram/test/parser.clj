(ns dram.test.parser
  (:require [dram.parser :as p]
            [clojure.test :refer :all]
            [the.parsatron :as parsatron :refer [run]]))


; Convenience Macros ----------------------------------------------------------
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


; AST Element Shortcuts -------------------------------------------------------
(defn b [name contents]   ; Block
  {:type :block :name name :contents contents})

(defn v [base filters]    ; Value
  {:type :value :base base :filters filters})

(defn f [path args]       ; Filter
  {:path path :args args})

(defn i [path args]       ; Innards
  {:path path :args args})

(defn it [path args]      ; Inline Tag
  {:type :inline-tag :path path :args args})

(defn ct [extends blocks] ; Child Template
  {:type :child :extends extends :blocks blocks})

(defn bt [contents]       ; Base Template
  {:type :base :contents contents})


; Tests -----------------------------------------------------------------------
(deftest optional-test
  (testing-parser
    (p/optional (p/literal-integer))
    "The optional parser makes another parser optional!"

    "42" 42
    "a"  nil))

(deftest separated-test
  (testing-parser
    (p/separated1 (parsatron/digit) (parsatron/char \,))
    "The separated1 parser parses items separated by things."

    "1"     [\1]
    "1,2"   [\1 \2]
    "1,2,3" [\1 \2 \3])

  (testing-parser
    (p/separated1 (p/literal) (parsatron/char \,))
    "Separated items can be complex."

    "1"                  [1]
    "42,\"hello world\"" [42 "hello world"])

  (testing-parser-errors
    (p/separated1 (p/literal) (parsatron/char \,))
    "The separated1 parser requires at least one item."

    ""
    ",")

  (testing-parser-errors
    (parsatron/>>
      (p/separated1 (p/literal) (parsatron/char \,))
      (parsatron/eof))
    "The separated1 parser doesn't allow garbage."

    "dogs"
    "1,,2"
    "1 2")

  (testing-parser
    (p/separated (parsatron/digit) (parsatron/char \,))
    "The separated parser is like separated1, but allows zero-length seqs."

    "1"     [\1]
    "1,2"   [\1 \2]
    ""      [])

  (testing-parser
    (parsatron/>>
      (p/separated (parsatron/digit) (parsatron/char \,))
      (parsatron/char \!))
    "The separated parser does not consume input if it fails."

    "1!"   \!
    "1,2!" \!
    "!"    \!)

  (testing-parser
    (parsatron/either
      (p/separated1 (parsatron/digit) (parsatron/char \,))
      (parsatron/char \!))
    "The separated1 parser does not consume input if it fails."

    "1!"   [\1]
    "1,2!" [\1 \2]
    "!"    \!)

  (testing-parser
    (parsatron/let->> [a (p/separated (parsatron/digit) (parsatron/char \,))
                       b (parsatron/char \,)]
                      (parsatron/always [a b]))
    "A more complicated example of input consumption."

    "1,2," [[\1 \2] \,]
    "1,"   [[\1] \,]
    ","    [[] \,]))

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

(deftest value-test
  (testing-parser
    (p/value)
    "A context value's base can be a path or a literal."

    "42"         (v 42 [])
    "\"foo\""    (v "foo" [])
    "user.email" (v ["user" "email"] [])
    "users.0"    (v ["users" "0"] []))

  (testing-parser
    (p/value)
    "A context value can be filtered through one or more filters."

    "42|abs"        (v 42 [(f ["abs"] [])])
    "42|math.floor" (v 42 [(f ["math" "floor"] [])])

    "\"foo\"|reverse|upper"
    (v "foo" [(f ["reverse"] [])
              (f ["upper"] [])])

    "\"foo\"|reverse|upper|custom.dogs"
    (v "foo" [(f ["reverse"] [])
              (f ["upper"] [])
              (f ["custom" "dogs"] [])]))

  (testing-parser
    (p/value)
    "Filters can take arguments."

    "total|add:extra.widgets"
    (v ["total"] [(f ["add"] [["extra" "widgets"]])])

    "description|trim:10"
    (v ["description"] [(f ["trim"] [10])])

    "description|slice:0,30"
    (v ["description"] [(f ["slice"] [0 30])])

    "user.join-date|date:\"yyyy-mm\",\"est\""
    (v ["user" "join-date"] [(f ["date"] ["yyyy-mm" "est"])])

    "number_of_cats|pluralize:\"y,ies\""
    (v ["number_of_cats"] [(f ["pluralize"] ["y,ies"])])

    "foo|join:\",\"|strip:\" ,.{}\"|slice:20,30,2|length"
    (v ["foo"] [(f ["join"] [","])
                (f ["strip"] [" ,.{}"])
                (f ["slice"] [20 30 2])
                (f ["length"] [])])))

(deftest variable-test
  (testing-parser
    (p/variable) "Variables can be simple literals."

    "{{ 42 }}"      (v 42 [])
    "{{ -2 }}"      (v -2 [])
    "{{ \"foo\" }}" (v "foo" []))

  (testing-parser
    (p/variable)
    "Variables can be values."

    "{{ dogs }}"          (v ["dogs"] [])
    "{{ user|is_admin }}" (v ["user"] [(f ["is_admin"] [])])

    "{{ \"sjl\"|is_admin }}"
    (v "sjl" [(f ["is_admin"] [])])

    "{{ user.username|slice:2,4 }}"
    (v ["user" "username"] [(f ["slice"] [2 4])])

    "{{ a|horrible:\"{{thing}}\" }}"
    (v ["a"] [(f ["horrible"] ["{{thing}}"])]))

  (testing-parser
    (p/variable) "Variables can handle wonky whitespace."

    "{{42}}"         (v 42 [])
    "{{ 42}}"        (v 42 [])
    "{{42 }}"        (v 42 [])
    "{{42  }}"       (v 42 [])
    "{{\n\t\n\t42}}" (v 42 []))

  )

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

    "{% block foo %}{% endblock %}" (b "foo" []))

  (testing-parser
    (p/tag-block)
    "Blocks can contain anything except other blocks."

    "{% block foo %}hi{% endblock %}"
    (b "foo" ["hi"])

    "{% block foo %}hi {{ 1 }} five{% endblock %}"
    (b "foo" ["hi " (v 1 []) " five"])

    "{% block foo %}
    {{ user.name|capitalize|trim:10 }}
    {% endblock %}"
    (b "foo" ["\n    "
              (v ["user" "name"] [(f ["capitalize"] [])
                                  (f ["trim"] [10])])
              "\n    "])))

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
    "  { foo }" "  { foo }"
    "..\"q\".." "..\"q\"..")

  (testing-parser
    (p/template-chunk) "A template chunk can be a variable."

    "{{ 1 }}"    (v 1 [])
    "{{ cats }}" (v ["cats"] [])))

(deftest template-base-test
  (testing-parser
    (p/template-base)
    "A base template can be made up of raw text, variables, ...."

    ""                        (bt [])
    "Hello"                   (bt ["Hello"])
    "Hello {{ \"Steve\" }}"   (bt ["Hello " (v "Steve" [])])
    "Age: {{ 27 }} years old" (bt ["Age: " (v 27 []) " years old"]))

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
         (v 42 [])])))

(deftest template-child-test
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
             "bar" [(v 10 [])]})))

(deftest template-tag-arguments
  (testing-parser
    (p/ttag-generic-arg)
    "Template tag arguments are usually left to the tags to parse/handle."

    "foo"     "foo"
    "foo.bar" "foo.bar"
    "<="      "<="
    "1234"    "1234"
    "foo,bar" "foo,bar"
    "{dogs}"  "{dogs}"
    "'lisp"   "'lisp")

  (testing-parser
    (p/ttag-arg)
    "Template tag arguments special case strings though."

    "\"foo\""            {:string "foo"}
    "\"a \\\"b\\\" c\""  {:string "a \"b\" c"}
    "\"boots and cats\"" {:string "boots and cats"}
    "\"{% okay %}\""     {:string "{% okay %}"})

  (testing-parser-errors
    (parsatron/>>
      (p/ttag-arg)
      (parsatron/eof))
    "Non-string tag arguments can't contain whitespace, quotes, or end chars."

    "foo\"bar"
    "foo bar"
    "f%}oo")


  )

(deftest template-tag-innards
  (testing-parser
    (p/ttag-innards)
    "Template tag innards are made of a required path and optional arguments."

    "foo"         (i ["foo"] [])
    "foo bar"     (i ["foo"] ["bar"])
    "foo cats 10" (i ["foo"] ["cats" "10"])
    "if  x <  10" (i ["if"] ["x" "<" "10"])

    "if username == \"sjl\""
    (i ["if"] ["username" "==" {:string "sjl"}])))

(deftest template-tag-inline
  (testing-parser
    (p/ttag-inline)
    "Inline template tags stand alone, require a path, and have optional args."

    "{% test %}"             (it ["test"] [])
    "{%test%}"               (it ["test"] [])
    "{% forms.csrf_token %}" (it ["forms" "csrf_token"] [])
    "{% messages user %}"    (it ["messages"] ["user"])

    "{% if x < 20
    and user.username == \"sjl\"
    and request.user|is_admin %}"
    (it ["if"] ["x" "<" "20"
                "and" "user.username" "==" {:string "sjl"}
                "and" "request.user|is_admin"])

    "{% very.fun \"{% tag %}\" %}"
    (it ["very" "fun"] [{:string "{% tag %}"}]))

  (testing-parser-errors
    (p/ttag-inline)
    "Inline template tags must start with a valid tag name."

    "{%"
    "%}"
    "{%%}"
    "{% %}"
    "{% >> %}"
    "{% ] %}"
    "{% { %}"
    "{% ,dogs %}"
    "{% .dogs %}"
    "{% \"if\" %}"
    "{% 'lisp %}"
    "{% :keywords-are-not-valid-path-names-for-now %}"))
