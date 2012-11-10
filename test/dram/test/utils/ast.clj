(ns dram.test.utils.ast)

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

