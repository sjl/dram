(ns dram.test.rendering
  (:require [dram.rendering :as r]
            [dram.test.utils.ast :as ast]
            [clojure.test :refer :all]))


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
         ["0" "0"]     [[:goal]]
         ))
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

(deftest render-base-simple-test
  (testing "Simple, string-only base templates should be trivial."
    (are [template context result] (= result (r/render-string template context))
         "foo" {}     "foo"
         ""    {}     ""
         ""    {:a 1} ""
         "foo" {:a 1} "foo"))
  (testing "Base templates without blocks should be pretty simple too."
    (are [template context result] (= result (r/render-string template context))
         "a {{ 1 }} b"       {} "a 1 b"
         "a {{ \"and\" }} b" {} "a and b"
         "{{ 1}}{{2}}"       {} "12"

         "Hello, {{ user.name }}!" {:user {:name "Steve"}}   "Hello, Steve!"
         "Hello, {{ user.name }}!" {:user {"name" "Steve"}}  "Hello, Steve!"
         "Hello, {{ user.name }}!" {"user" {:name "Steve"}}  "Hello, Steve!"
         "Hello, {{ user.name }}!" {"user" {"name" "Steve"}} "Hello, Steve!"

         "{{ a.1 }}, {{a.2}}" {:a "xyz"}               "y, z"
         "{{ a.0 }}, {{a.2}}" {:a ["foo" "bar" "baz"]} "foo, baz"
         "{{ a.0 }}, {{a.2}}" {:a {0 :q "2" :r}}       ":q, :r"
         )))
