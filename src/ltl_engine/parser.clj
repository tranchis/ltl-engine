(ns ltl-engine.parser
  (:require [instaparse.core :as insta]))

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

(declare tree->tree)

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
