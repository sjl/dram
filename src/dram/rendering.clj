(ns dram.rendering
  (:require [dram.parser :refer [parse]]
            [dram.context :refer [lookup]]))


(defn render-block [block context])

(defn render-value [{:keys [base filters]} context]
  (let [v (if (or (number? base) (string? base))
            base
            (lookup base context))]
    v))

(defn render-chunk [chunk context]
  (cond
    (string? chunk) chunk
    (= :value (:type chunk)) (str (render-value chunk context))
    (= :block (:type chunk)) (render-block chunk context)))

(defn render-base [{:keys [contents]} context]
  (apply str (map #(render-chunk % context) contents)))

(defn render-child [ast context])

(defn render-ast [ast context]
  (case (:type ast)
    :base (render-base ast context)
    :child (render-child ast context)))

(defn render-string [raw-template context]
  (render-ast (parse raw-template) context))
