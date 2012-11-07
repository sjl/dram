(ns dram.test.parser
  (:require [dram.parser :as p]
            [clojure.test :refer :all]
            [the.parsatron :as tron :refer [run defparser let->> >>]]))

; Extra Utility Parsers -------------------------------------------------------
(defparser complete [p]
  (let->> [r p _ (tron/eof)]
    (tron/always r)))

(defparser any-string []
  (let->> [r (tron/many1 (tron/any-char))]
    (tron/always (apply str r))))


; Convenience Macros ----------------------------------------------------------
(defmacro is-error [input parser]
  `(~'is (~'thrown? RuntimeException (run ~parser ~input))))

(defmacro parses-as [input parser output]
  `(~'is (~'= (run ~parser ~input) ~output)))

(defmacro testing-parser [parser desc & data]
  (let [p (gensym "parser")
        pairs (partition 2 data)]
    `(let [~p (complete ~parser)]
       (~'testing ~desc
          ~@(map (fn [[input output]]
                   `(parses-as ~input ~p ~output))
                 pairs)))))

(defmacro testing-parser-errors [parser desc & data]
  (let [p (gensym "parser")]
    `(let [~p (complete ~parser)]
       (~'testing ~desc
          ~@(map (fn [input]
                   `(is-error ~input ~p))
                 data)))))


; AST Element Shortcuts -------------------------------------------------------
(defn e  ; Extends
  ([path]
   {:type :extends :path path}))

(defn b  ; Block
  ([name]
   (b name []))
  ([name contents]
   {:type :block :name name :contents contents}))

(defn v  ; Value
  ([base]
   (v base []))
  ([base filters]
   {:type :value :base base :filters filters}))

(defn f  ; Filter
  ([path]
   (f path []))
  ([path args]
   {:path path :args args}))

(defn i  ; Innards
  ([path]
   (i path []))
  ([path args]
   {:path path :args args}))

(defn it ; Inline Tag
  ([path]
   (it path []))
  ([path args]
   {:type :inline-tag :path path :args args}))

(defn ct ; Child Template
  ([extends]
   (ct extends {}))
  ([extends blocks]
   {:type :child :extends extends :blocks blocks}))

(defn bt ; Base Template
  ([contents]
   {:type :base :contents contents}))


; Tests -----------------------------------------------------------------------
(deftest optional-test
  (testing-parser
    (p/optional (p/literal-integer))
    "The optional parser makes another parser optional!"

    "42" 42
    ""  nil))

(deftest separated1-test
  (testing-parser
    (p/separated1 (tron/digit) (tron/char \,))
    "The separated1 parser parses items separated by things."

    "1"     [\1]
    "1,2"   [\1 \2]
    "1,2,3" [\1 \2 \3])

  (testing-parser
    (p/separated1 (p/literal) (tron/char \,))
    "Separated items can be complex."

    "1"                   [1]
    "\"foo\""             ["foo"]
    "42,\"hello, world\"" [42 "hello, world"]
    "19,\"foo\",1"        [19 "foo" 1])

  (testing-parser-errors
    (p/separated1 (p/literal) (tron/char \,))
    "The separated1 parser requires at least one item."

    ""
    ","
    "a")

  (testing-parser-errors
    (p/separated1 (p/literal) (tron/char \,))
    "The separated1 parser doesn't allow garbage."

    "dogs"
    "1,,2"
    "1 2"
    ",1"
    "1,2,3,")

  (testing-parser
    (tron/either
      (p/separated1 (tron/digit) (tron/char \,))
      (tron/char \!))
    "The separated1 parser does not consume input if it fails."

    "1"   [\1]
    "1,2" [\1 \2]
    "!"   \!)

  (testing-parser
    (let->> [a (p/separated1 (tron/digit) (tron/char \,))
             b (tron/char \,)]
      (tron/always [a b]))
    "A more complicated example of input consumption."

    "1,2," [[\1 \2] \,]
    "1,"   [[\1] \,]))

(deftest separated-test
  (testing-parser
    (p/separated (tron/digit) (tron/char \,))
    "The separated parser is like separated1, but allows zero-length seqs."

    "1"     [\1]
    "1,2"   [\1 \2]
    ""      [])

  (testing-parser
    (p/separated (p/literal) (tron/char \,))
    "Separated items can be complex."

    "1"                   [1]
    "\"foo\""             ["foo"]
    "42,\"hello, world\"" [42 "hello, world"]
    "19,\"foo\",1"        [19 "foo" 1])

  (testing-parser-errors
    (p/separated (p/literal) (tron/char \,))
    "The separated parser doesn't allow garbage."

    "dogs"
    "1,,2"
    "1 2"
    ",1"
    "1,2,3,")

  (testing-parser
    (>>
      (p/separated (tron/digit) (tron/char \,))
      (tron/char \!))
    "The separated parser does not consume input if it fails."

    "1!"   \!
    "1,2!" \!
    "!"    \!)

  (testing-parser
    (let->> [a (p/separated (tron/digit) (tron/char \,))
             b (tron/char \,)]
      (tron/always [a b]))
    "A more complicated example of input consumption."

    "1,2," [[\1 \2] \,]
    "1,"   [[\1] \,]
    ","    [[] \,]))

(deftest whitespace-test
  (testing-parser
    (p/optional-whitespace) "Optional whitespace parses to nil."

    ""       nil
    " "      nil
    "     "  nil
    "\t"     nil
    "\n"     nil
    "\n \t " nil)

  (testing-parser
    (>> (p/optional-whitespace) (p/literal))
    "Optional whitespace doesn't consume input on failure."

    "  1" 1
    "1"   1)

  (testing-parser
    (p/required-whitespace) "Required whitespace parses to nil."

    " "      nil
    "     "  nil
    "\t"     nil
    "\n"     nil
    "\n \t " nil)

  (testing-parser-errors
    (p/required-whitespace) "Required whitespace is actually required."

    ""
    "foo")

  (testing-parser
    (tron/either (p/required-whitespace) (p/literal))
    "Required whitespace can't consume input on failure."

    "1" 1))

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
    "--1"
    " -1"
    "- 1"
    "-a12")

  (testing-parser
    (tron/either (p/literal-integer) (any-string))
    "Parsing integers does not consume data on failure."

    "-1"  -1
    "--1" "--1"
    "c4t" "c4t"))

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
    "foo\""
    "fo\"o")

  (testing-parser
    (tron/either (p/literal-string) (any-string))
    "Strings don't consume input on failure."

    "aaa"   "aaa"
    "\"a"   "\"a"))

(deftest literal-test
  (testing-parser
    (p/literal) "Literals can parse integers."

    "-42" -42
    "585" 585)

  (testing-parser
    (p/literal) "Literals can parse strings."

    "\"foo\"" "foo")

  (testing-parser
    (tron/either (p/literal) (any-string))
    "Literals don't consume input on failure."

    "-cats" "-cats"
    "--1"   "--1"
    "\"foo" "\"foo"))

(deftest value-test
  (testing-parser
    (p/value)
    "A context value's base can be a path or a literal."

    "42"         (v 42)
    "\"foo\""    (v "foo")
    "user.email" (v ["user" "email"])
    "users.0"    (v ["users" "0"]))

  (testing-parser
    (p/value)
    "A context value can be filtered through one or more filters."

    "42|abs"        (v 42 [(f ["abs"])])
    "42|math.floor" (v 42 [(f ["math" "floor"])])

    "\"foo\"|reverse|upper"
    (v "foo" [(f ["reverse"])
              (f ["upper"])])

    "\"foo\"|reverse|upper|custom.dogs"
    (v "foo" [(f ["reverse"])
              (f ["upper"])
              (f ["custom" "dogs"])]))

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
                (f ["length"])])))

(deftest variable-test
  (testing-parser
    (p/variable) "Variables can be simple literals."

    "{{ 42 }}"      (v 42)
    "{{ -2 }}"      (v -2)
    "{{ \"foo\" }}" (v "foo"))

  (testing-parser
    (p/variable)
    "Variables can be values."

    "{{ dogs }}"          (v ["dogs"])
    "{{ user|is_admin }}" (v ["user"] [(f ["is_admin"])])

    "{{ \"sjl\"|is_admin }}"
    (v "sjl" [(f ["is_admin"])])

    "{{ user.username|slice:2,4 }}"
    (v ["user" "username"] [(f ["slice"] [2 4])])

    "{{ a|horrible:\"{{thing}}\" }}"
    (v ["a"] [(f ["horrible"] ["{{thing}}"])]))

  (testing-parser
    (p/variable) "Variables can handle wonky whitespace."

    "{{42}}"         (v 42)
    "{{ 42}}"        (v 42)
    "{{42 }}"        (v 42)
    "{{42  }}"       (v 42)
    "{{\n\t\n\t42}}" (v 42))

  )

(deftest extends-test
  (testing-parser
    (p/tag-extends)
    "{% extends ... %} parses to its own custom AST element."

    "{% extends \"p\" %}"       (e "p")
    "{% extends \"foo/bar\" %}" (e "foo/bar"))

  (testing-parser
    (p/tag-extends)
    "{% extends ... %} can take weird whitespace."

    "{%extends \"foo\"%}"          (e "foo")
    "{% extends \"foo\"%}"         (e "foo")
    "{%extends \"foo\" %}"         (e "foo")
    "{%\n  \n\textends\n\"foo\"%}" (e "foo"))

  (testing-parser-errors
    (p/tag-extends)
    "{% extends ... %} requires a non-empty argument."

    "{% extends \"\" %}"
    "{% extends %}")

  (testing-parser-errors
    (p/tag-extends)
    "{% extends ... %} doesn't accept garbage."

    "{% extends\"foo\" %}"
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

    "{% block foo %}{% endblock %}" (b "foo"))

  (testing-parser
    (p/tag-block)
    "Blocks can contain anything except other blocks."

    "{% block foo %}hi{% endblock %}"
    (b "foo" ["hi"])

    "{% block foo %}hi {{ 1 }} five{% endblock %}"
    (b "foo" ["hi " (v 1) " five"])

    "{% block foo %}
    {{ user.name|capitalize|trim:10 }}
    {% endblock %}"
    (b "foo" ["\n    "
              (v ["user" "name"] [(f ["capitalize"])
                                  (f ["trim"] [10])])
              "\n    "])))

(deftest raw-text-test
  (testing-parser
    (p/raw-text) "Raw text parses to a vanilla Clojure string."

    "Hello"             "Hello"
    "hello there world" "hello there world"
    "  { foo } is okay" "  { foo } is okay"
    "so is { % foo % }" "so is { % foo % }")

  (testing-parser-errors
    (p/raw-text) "Reserved characters do not parse as raw text."

    "Hello{{ world }}"
    "Hello{% block world %}"
    "Hello{% there"
    "Hello{{world")

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
    "foo/"
    ".a"
    "..a"
    "a..b"
    "a$b"
    "a,b"
    "a b"
    "a."
    "a.b."
    ":"
    ":::")

  (testing-parser-errors
    (p/path) "A path can't contain keyword-style args (for now)."

    ":foo"
    "foo.:bar"))


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
    (>>
      (p/ttag-arg)
      (tron/eof))
    "Non-string tag arguments can't contain whitespace, quotes, or end chars."

    "foo\"bar"
    "foo bar"
    "f%}oo"))

(deftest template-tag-innards
  (testing-parser
    (p/ttag-innards)
    "Template tag innards are made of a required path and optional arguments."

    "foo"         (i ["foo"])
    "foo bar"     (i ["foo"] ["bar"])
    "foo cats 10" (i ["foo"] ["cats" "10"])
    "if  x <  10" (i ["if"] ["x" "<" "10"])

    "if username == \"sjl\""
    (i ["if"] ["username" "==" {:string "sjl"}])

    "this \"{{ is }}\" \"{%horrible%}\""
    (i ["this"] [{:string "{{ is }}"}
                 {:string "{%horrible%}"}])))

(deftest template-tag-inline
  (testing-parser
    (p/ttag-inline)
    "Inline template tags stand alone, require a path, and have optional args."

    "{% test %}"             (it ["test"])
    "{%test%}"               (it ["test"])
    "{% forms.csrf_token %}" (it ["forms" "csrf_token"])
    "{% messages user %}"    (it ["messages"] ["user"])

    "{% if x < 20
    and user.username == \"sjl\"
    and request.user|is_admin %}"
    (it ["if"] ["x" "<" "20"
                "and" "user.username" "==" {:string "sjl"}
                "and" "request.user|is_admin"])

    "{% very.fun \"{% tag %}\" %}"
    (it ["very" "fun"] [{:string "{% tag %}"}]))

  (testing-parser
    (p/ttag-inline)
    "Inline template tags can contain not-quite-reserved words."

    "{% blocked %}"      (it ["blocked"])
    "{% block.test %}"   (it ["block" "test"])
    "{% extends-with %}" (it ["extends-with"]))

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
    "{% :keywords-are-not-valid-path-names-for-now %}"
    "{% extends %}"
    "{% extends 1 %}"
    "{% block %}"
    "{% block. %}"
    "{% endblock %}"
    "{% end.block %}"
    "{% endif %}"
    "{% end %}"
    "{% load %}"
    "{% load \"foo\" %}"))


(deftest template-chunk-test
  (testing-parser
    (p/template-chunk) "A template chunk can be raw text."

    "Hello"     "Hello"
    "  { foo }" "  { foo }"
    "..\"q\".." "..\"q\"..")

  (testing-parser
    (p/template-chunk) "A template chunk can be a variable."

    "{{ 1 }}"                 (v 1)
    "{{ \"sjl\"|uppercase }}" (v "sjl" [(f ["uppercase"])])
    "{{ cats.avy }}"          (v ["cats" "avy"])
    "{{ cats|names:10 }}"     (v ["cats"] [(f ["names"] [10])]))

  (testing-parser
    (p/template-chunk) "A template chunk can be an inline tag."

    "{% a %}"       (it ["a"])
    "{% a.b %}"     (it ["a" "b"])
    "{% a.b foo %}" (it ["a" "b"] ["foo"])

    "{% show-form-unless x < 10 or username == \"sjl\" %}"
    (it ["show-form-unless"] ["x" "<" "10" "or" "username" "=="
                              {:string "sjl"}]))

  (testing-parser-errors
    (p/template-chunk) "A template chunk cannot be zero-length."

    ""))

(deftest template-base-test
  (testing-parser
    (p/template-base)
    "A base template can be made up of a series of chunks."

    ""                        (bt [])
    "Hello"                   (bt ["Hello"])
    "Hello {{ \"Steve\" }}"   (bt ["Hello " (v "Steve")])
    "Age: {{ 27 }} years old" (bt ["Age: " (v 27) " years old"])

    "Hello {% get-name %}!"
    (bt ["Hello " (it ["get-name"]) "!"])

    "{% greeting %}{{ user.name }}!"
    (bt [(it ["greeting"]) (v ["user" "name"]) "!"]))

  (testing-parser
    (p/template-base)
    "A base template can contain blocks."

    "{% block foo %}{% endblock %}"
    (bt [(b "foo")])

    "hello {% block username %}{% endblock %}"
    (bt ["hello "
         (b "username")])

    "hello {% block username %}{% endblock %}! {% block rest %}{% endblock %}"
    (bt ["hello "
         (b "username")
         "! "
         (b "rest")])

    "foo {% block a %}{% endblock %} bar {{ 42 }}"
    (bt ["foo "
         (b "a")
         " bar "
         (v 42)])))

(deftest template-child-test
  (testing-parser
    (p/template-child)
    "A child template requires an extends tag."

    "{% extends \"a\" %}"     (ct "a")
    "  {% extends \"a\" %}"   (ct "a")
    "{% extends \"a\" %}\n\n" (ct "a"))

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
             "bar" [(v 10)]})))

