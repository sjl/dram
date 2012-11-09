(ns dram.core
  (:require [dram.parser :refer [parse]]
            [dram.context :refer [lookup]]))


(defn render-block [block context])

(defn render-value [{:keys [base filters]} context]
  (let [v (cond
            (number? base) (str base)
            (string? base) base
            (seq base) (lookup base context))]
    v))

(defn render-chunk [chunk context]
  (cond
    (string? chunk) chunk
    (= :value (:type chunk)) (render-value chunk context)
    (= :block (:type chunk)) (render-block chunk context)))

(defn render-base [{:keys [contents]} context]
  (apply str (map #(render-chunk % context) contents)))

(defn render-child [ast context])

(defn render-ast [ast context]
  (case (:type ast)
    :base-template (render-base ast context)
    :child-template (render-child ast context)))

(defn render-string [raw-template context]
  (render-ast (parse raw-template) context))
