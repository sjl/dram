(ns dram.test.rendering
  (:require [dram.rendering :as r]
            [dram.parser :refer [parse]]
            [dram.test.utils.ast :as ast]
            [clojure.test :refer :all]))


(deftest slurp-test
  (testing "Slurping should do its best to get the template."
    (are [path content] (= (r/slurp-template path) content)
         "dram-test/sample.dram"           "this is a sample template\n"
         "templates/dram-test/sample.dram" "this is a sample template\n")))

(deftest render-value-unfiltered-test
  (testing "Literals render to themselves."
    (are [base context result] (= result (r/render-value (ast/v base) context))
         1     {}     1
         "foo" {}     "foo"
         ""    {}     ""
         0     {0 :a} 0
         0     [:a]   0
         -1    {}     -1))
  (testing "Paths look things up in the context."
    (are [base context] (= :goal (r/render-value (ast/v base) context))
         ; Flat contexts are fine.
         ["a"]  {:a :goal}
         ["a"]  {"a" :goal}
         ["0"]  {0 :goal}
         ["1"]  {1 :goal}
         ["-1"] {-1 :goal}
         ["0"]  {:0 :goal}
         ["1"]  {:1 :goal}
         ["-1"] {:-1 :goal}
         ["0"]  {"0" :goal}
         ["1"]  {"1" :goal}
         ["-1"] {"-1" :goal}
         ["a"]  {:b 0 :a :goal}
         ; Vectors can be contexts too.
         ["0"] [:goal :b :c]
         ["1"] [:a :goal :c]
         ; Multi-level contexts should work too.
         ["a" "b"]     {:a {:b :goal}}
         ["a" "0"]     {:a {0 :goal}}
         ["a" "0"]     {:a [:goal]}
         ["a" "b" "c"] {:a {"b" {"c" :goal}}}
         ["a" "b" "c"] {:a {"b" {:c :goal}}}
         ["0" "0"]     [[:goal]]))
  (testing "Paths can bail early if a segment isn't found."
    (are [base context] (nil? (r/render-value (ast/v base) context))
         ["a"]     {}
         ["a"]     []
         ["a"]     {:b 0}
         ["a" "b"] {:a 0}
         ["a" "b"] {:b 0}
         ["a" "b"] {:a {:c 0}}
         ["a" "b"] {:a [1 2 3]}
         ["a" "b"] {:q {:b 0}}
         ["-1"]    [:a]
         ["0" "1"] [[:a]]
         ["1" "0"] [[:a]])))

(deftest render-chunk-simple-test
  (testing "Strings are passed straight through when rendered as chunks."
    (are [input context result] (= result (r/render-chunk input context))
         ""    {}     ""
         ""    {:a 1} ""
         "foo" {}     "foo"))
  (testing "Values get cast to strings on the way out."
    (are [input context result] (= result (r/render-chunk (ast/v input) context))
         1     {}       "1"
         -1    {}       "-1"
         "foo" {}       "foo"
         ["a"] {:a nil} ""
         ["a"] {:a 1}   "1"
         ["a"] {:a :b}  ":b"
         ["a"] {:a "x"} "x")))

(deftest render-block-test
  (testing "AST blocks should render into an intermediate [name str] form."
    (are [name contents context result]
         (= result (r/render-block (ast/b name contents) context))
         "body" ["foo" "bar"]          {} ["body" "foobar"]
         "body" ["meow" (ast/v 10)]    {} ["body" "meow10"]
         "body" ["meow" (ast/v ["a"])] {} ["body" "meow"]

         "sample"
         ["username:" (ast/v ["user" "username"])]
         {:user {:username "sjl"}}
         ["sample" "username:sjl"])))

(deftest render-base-test
  (testing "Simple base templates render into [[name str] ...] seqs."
    (are [template context result]
         (= result (r/render-base (parse template) context))

         "foo"
         {}
         [[nil "foo"]]

         "foo {{ 1 }} bar"
         {}
         [[nil "foo "]
          [nil "1"]
          [nil " bar"]]

         "foo {% block body %}body {{ 1 }} text{% endblock %} bar"
         {}
         [[nil "foo "]
          ["body" "body 1 text"]
          [nil " bar"]]

         (apply str
                "{% block a %}a: {{ a.val }}{% endblock %}"
                "{% block b %}b: {{ b.val }}{% endblock %}")
         {:a {:val 1}}
         [["a" "a: 1"]
          ["b" "b: "]])))


(deftest full-template-test
  (testing "Send templates all the way through the pipeline."
    (are [path context result] (= (r/render-template path context) result)
         "dram-test/sample.dram" {} "this is a sample template\n"
         "dram-test/literals.dram" {} "1\ncats\n"

         "dram-test/user.dram" {} "Hello, !\n"

         "dram-test/user.dram"
         {:user {"username" "sjl"}}
         "Hello, sjl!\n"

         "dram-test/base.dram" {} "<html><head></head></html>\n"
         )
    )

  )
