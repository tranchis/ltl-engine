(ns ltl-engine.core
  (:require [instaparse.core :as insta]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

(def ltl-parser
  (insta/parser
   "S := T | TRUE | FALSE | N | A | O | I | U | R | X | F | G | <'('> <SEP> S <SEP> <')'>
    TRUE := <'TRUE'>
    FALSE := <'FALSE'>
    N := <'NOT'> <W> S
    A := S <W> <'AND'> <W> S
    O := S <W> <'OR'> <W> S
    I := S <W> <'IMPLIES'> <W> S
    U := S <W> <'UNTIL'> <W> S
    R := S <W> <'RELEASE'> <W> S
    X := <'NEXT'> <W> S
    F := <'EVENTUALLY'> <W> S
    G := <'GLOBALLY'> <W> S
    SEP := #'\\s*'
    W := #'\\s+' 
    T := #'[a-z]+'"))

(defn parse-ltl [s] (insta/parse ltl-parser s))

(defmulti neg-tree->tree first)
(defmethod neg-tree->tree nil [_] (throw (UnsupportedOperationException.)))
(defmethod neg-tree->tree :N [[_ n]] (tree->tree n))
(defmethod neg-tree->tree :T [[_ t]] [:N t])
(defmethod neg-tree->tree :S [[_ s]] (neg-tree->tree s))
(defmethod neg-tree->tree :F [[_ s]] [:R :FALSE (neg-tree->tree s)])
(defmethod neg-tree->tree :G [[_ s]] [:U :TRUE (neg-tree->tree s)])
(defmethod neg-tree->tree :I [[_ s1 s2]] [:A (tree->tree s1) (neg-tree->tree s2)])
(defmethod neg-tree->tree :O [[_ s1 s2]] [:A (neg-tree->tree s1) (neg-tree->tree s2)])
(defmethod neg-tree->tree :A [[_ s1 s2]] [:O (neg-tree->tree s1) (neg-tree->tree s2)])

(defmulti tree->tree first)
(defmethod tree->tree nil [_] [])
(defmethod tree->tree :N [[_ n]] (neg-tree->tree n))
(defmethod tree->tree :T [[_ t]] t)
(defmethod tree->tree :S [[_ s]] (tree->tree s))
(defmethod tree->tree :F [[_ s]] [:U :TRUE (tree->tree s)])
(defmethod tree->tree :G [[_ s]] [:R :FALSE (tree->tree s)])
(defmethod tree->tree :I [[_ s1 s2]] [:O [:N (tree->tree s1)] (tree->tree s2)])
(defmethod tree->tree :default [[k & ltls]]
  (concat [k] (into [] (map tree->tree ltls))))

(defmulti cl first)

(defn negate [ltl]
  (let [[k & ss] ltl]
    (condp = k
      :N (first ss)
      :TRUE [:FALSE]
      :FALSE [:TRUE]
      [:N ltl])))

(defn cl [ltl]
  (when-not (= [] ltl)
    (if (string? ltl)
      #{ltl (negate ltl)}
      (let [[k & ss] ltl]
        (apply set/union #{ltl (negate ltl)} (map cl ss))))))

(defn cl-f [ltl]
  (set/union #{[:TRUE] [:FALSE]} (cl ltl)))

(defn check-ors [cl-set m f]
  (let [all-ors (into #{}
                      (filter #(and (= (first %) :O)
                                    (or (= (second %) f)
                                        (= (nth % 2) f)))
                              cl-set))]
    (= all-ors (set/intersection all-ors m))))

(defn is-maximal? [cl-set c]
  (let [m (into #{} c)
        has-true? (contains? m [:TRUE])
        r-and (fn [p n] (and p n))
        either? (reduce r-and true (map #(or
                                          (contains? m %)
                                          (contains? m (negate %)))
                                        cl-set))
        no-negate? (reduce r-and true (map #(not (contains? m (negate %))) m))
        fulfills-and? (reduce r-and true (map #(and (contains? m (second %))
                                                    (contains? m (nth % 2)))
                                              (filter #(= :A (first %)) m)))
        fulfills-reverse-and? (reduce r-and true
                                      (map #(or
                                             (and
                                              (not (contains? cl-set [:A (second %) (first %)]))
                                              (not (contains? cl-set [:A (first %) (second %)])))
                                             (contains? m [:A (first %) (second %)])
                                             (contains? m [:A (second %) (first %)]))
                                           (combo/combinations (remove #(= :TRUE (first %)) m) 2)))
        fulfills-or? (reduce r-and true (map #(or (contains? m (second %))
                                                  (contains? m (nth % 2)))
                                             (filter #(= :O (first %)) m)))
        fulfills-reverse-or? (reduce r-and true (map #(check-ors cl-set m %) m))]
    (and has-true? either? no-negate? fulfills-and? fulfills-or?
         fulfills-reverse-and? fulfills-reverse-or?)))

(defn cs [cl-set]
  (let [perms (combo/subsets cl-set)]
    (filter #(is-maximal? cl-set %) perms)))

#_(defn automata-1 [ltl]
  )

#_(defn gba [ltl]
  (let [cl (cl-f ltl)
        a1 (automata-1 ltl)
        a2 (automata-2 ltl)]
    nil))
