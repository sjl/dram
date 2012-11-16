(ns dram.rendering
  (:require [clojure.string :refer [join]]
            [dram.parser :refer [parse]]
            [dram.context :refer [lookup]]))


(defn find-template-url [path]
  (or (clojure.java.io/resource path)
      (clojure.java.io/resource (join java.io.File/separator
                                      ["templates" path]))))

(defn slurp-template [path]
  (slurp (find-template-url path)))


(declare render-ast render-string)

(defn render-value
  "Render an AST value node into a string according to the context."
  [{:keys [base filters]} context]
  (let [v (if (or (number? base) (string? base))
            base
            (lookup base context))]
    v))

(defn render-chunk
  "Render a chunk (a non-block) to a flat string."
  [chunk context]
  (cond
    (string? chunk) chunk
    (= :value (:type chunk)) (str (render-value chunk context))))

(defn render-contents
  "Render the contents of a block to a flat string."
  [contents context]
  (apply str (map #(render-chunk % context) contents)))

(defn render-block
  "Render an AST block to a [name content] pair."
  [{:keys [name contents]} context]
  [name (render-contents contents context)])

(defn render-base
  "Render a base template to a [[name content] ...] seq."
  [{:keys [contents]} context]
  (for [c contents]
    (if (= :block (get-in c [:type]))
      (render-block c context)
      [nil (render-chunk c context)])))

(defn render-child
  "Render a child template to a [[name content] ...] seq."
  [{:keys [extends blocks]} context]
  (let [parent-ast (render-string (slurp-template extends) context)
        parent (render-ast extends context)]
    (for [[name content] parent]
      (if-let [override (get blocks name)]
        [name (render-contents override context)]
        [name content]))))

(defn render-flatten
  "Flatten a [[name content] ...] seq into a single string."
  [blockpairs]
  (apply str (map second blockpairs)))

(defn render-ast
  "Render the given AST and context into a [[name content] ...] seq."
  [ast context]
  (case (:type ast)
    :base (render-base ast context)
    :child (render-child ast context)))

(defn render-string [raw-template context]
  (render-flatten (render-ast (parse raw-template) context)))

(defn render-template [path context]
  (render-string (slurp-template path) context))
