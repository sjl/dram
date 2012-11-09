(ns dram.test.context
  (:require [dram.context :as c]
            [clojure.test :refer :all]))


(deftest to-keys-test
  (testing "Strings parse to string, keyword, and possibly numeric keys."
    (are [s ks] (= ks (set (c/to-keys s)))
         "foo"        #{:foo "foo"}
         "a"          #{:a "a"}
         "boots-cats" #{:boots-cats "boots-cats"}
         "1"          #{:1 "1" 1}
         "a-1"        #{:a-1 "a-1"}
         "-1"         #{:-1 "-1" -1})))

(deftest try-keys-test
  (testing "try-keys finds the value if it exists in the context"
    (are [ks context value] (= value (c/try-keys ks context))
         [:foo :bar]  {:foo 1}   1
         [:foo :bar]  {:bar 1}   1
         ["a" :foo]   {"a" 1}    1
         [:foo "a" 0] {"a" 1}    1
         [1 :foo "a"] {"a" 1}    1
         [0]          [:a :b :c] :a
         [0 1]        [:a :b :c] :a
         [1 0]        [:a :b :c] :b
         [:foo 0]     [:a :b :c] :a))
  (testing "try-keys returns nil if it can't find the key in the context"
    (are [ks context] (= nil (c/try-keys ks context))
         [:foo :bar] {:dogs 1}
         [:bar]      {:dogs 1}
         []          {:dogs 1}
         [0]         {:dogs 1}
         [:foo "a"]  [1 2 3]
         [-1 4]      [1 2 3])))

(deftest lookup-test
  (testing "lookup finds things!"
    (are [path context] (= :goal (c/lookup path context))
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
  (testing "lookup returns nil if it can't find the path at any stage"
    (are [path context] (nil? (c/lookup path context))
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
