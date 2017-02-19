(ns ltl-engine.optimized
  (:require [ltl-engine.parser :as parser]
            [clojure.set :as set]
            [ltl-engine.core :as ltlc]
            [clojure.math.combinatorics :as combo]))

(def test-f (parser/tree->tree (parser/parse-ltl "a UNTIL b")))

(defn uuid [] (str (java.util.UUID/randomUUID)))

(defn new-name [] (uuid))

(defrecord GraphNode [name father incoming new old next])

(defn new-1 [[k f-1 f-2]]
  (condp = k
    :U #{f-1}
    :R #{f-2}
    :O #{f-1}
    (throw (UnsupportedOperationException.))))

(defn next-1 [[k f-1 f-2]]
  (condp = k
    :U #{[k f-1 f-2]}
    :R #{[k f-1 f-2]}
    :O #{}
    (throw (UnsupportedOperationException.))))

(defn new-2 [[k f-1 f-2]]
  (condp = k
    :U #{f-2}
    :R (into #{} [f-1 f-2])
    :O #{f-2}
    (throw (UnsupportedOperationException.))))

(defn expand [node nodes-set]
  (let [{:keys [name father incoming new old next]} node]
    (if (empty? new)
      (if-let [nd (first (filter #(and (= (:old %) old)
                                       (= (:next %) next))
                                 (vals nodes-set)))]
        (let [new-nd (update nd :incoming set/union incoming)]
          (assoc nodes-set (:name new-nd) new-nd))
        (let [nname (new-name)
              new-node (->GraphNode nname nname #{name} next #{} #{})
              new-set (assoc nodes-set name node)]
          (expand new-node new-set)))
      (let [f (first new)
            new-new (set/difference new #{f})]
        (if (or (string? f) (= :N (first f)) (= f [:TRUE]) (= f [:FALSE]))
          (if (or (= f [:FALSE]) (contains? old (ltlc/negate f)))
            nodes-set
            (let [new-old (set/union old #{f})
                  new-node (merge node {:old new-old :new new-new})]
              (expand new-node nodes-set)))
          (let [k (first f)]
            (if (#{:U :R :O} k)
              (let [new-1-new (set/union new-new (set/difference (new-1 f) old))
                    new-1-old (set/union old #{f})
                    new-1-next (set/union next (next-1 f))
                    new-2-new (set/union new-new (set/difference (new-2 f) old))
                    node-1 (->GraphNode (new-name) name incoming new-1-new
                                        new-1-old new-1-next)
                    node-2 (->GraphNode (new-name) name incoming new-2-new
                                        new-1-old next)]
                (expand node-2 (expand node-1 nodes-set)))
              (if (= k :A)
                (let [f-1 (second f)
                      f-2 (nth f 2)
                      new-node-new (set/union new-new
                                              (set/difference (into #{} [f-1 f-2])
                                                              old))
                      new-node-old (set/union old #{f})
                      new-node (->GraphNode name father incoming new-node-new
                                            new-node-old next)]
                  (expand new-node nodes-set))
                (if (= k :X)
                  (let [new-old (set/union old #{f})
                        new-next (set/union next #{(second f)})
                        new-node (->GraphNode name father incoming new-new
                                              new-old new-next)]
                    (expand new-node nodes-set))
                  (throw (UnsupportedOperationException. (pr-str f))))))))))))

(defn create-graph [ltl]
  (let [nname (new-name)]
    (expand (->GraphNode nname nname #{"init"} #{ltl} #{} #{}) {})))

(defn automaton [ltl]
  (let [g (create-graph ltl)
        cl-s (ltlc/cl-f ltl)
        ap (filter string? cl-s)
        ap-s (into #{} ap)
        ap-2 (combo/subsets ap)
        q (filter #(empty? (:new %)) (vals g))
        i (filter #(contains? (:incoming %) "init") q)
        transitions (apply merge
                           (map (fn [p]
                                  (hash-map
                                   p
                                   (into #{}
                                         (map :name
                                              (filter
                                               (fn [qq]
                                                 (contains? (:incoming qq) p))
                                               q)))))
                                (keys g)))
        l (apply merge
                 (map (fn [qq]
                        (let [pos-q (set/intersection (:old qq) ap-s)
                              neg-q (into #{}
                                          (filter #(contains? (:old qq) [:N %])
                                                  ap-s))]
                          {(:name qq)
                           (into #{}
                                 (filter (fn [p-2]
                                           (and (set/subset? pos-q p-2)
                                                (empty? (set/intersection p-2 neg-q))))
                                         (map #(into #{} %) ap-2)))}))
                      q))
        f (into #{}
                (map (fn [phi]
                       (into #{} (map :name
                                      (filter (fn [{:keys [old]}]
                                                (or (not (contains? old phi))
                                                    (contains? old (nth phi 2))))
                                              q))))
                     (filter #(= :U (first %)) cl-s)))]
    {:q q
     :i i
     :valid? true
     :states #{}
     :accepted? []
     :run []
     :transitions transitions
     :d ap-2
     :l l
     :f f}))

(defn properties [{:keys [q l f transitions]}]
  {:nodes (count q)
   :transitions (reduce + (map count (vals transitions)))
   :accepts (count f)})

(def test-ltl "GLOBALLY ((NOT p) IMPLIES (NEXT p))")
(def tests ["p UNTIL q"
            "p UNTIL (q UNTIL r)"
            "NOT (p UNTIL (q UNTIL r))"
            "(GLOBALLY (EVENTUALLY p)) IMPLIES (GLOBALLY (EVENTUALLY q))"
            "(EVENTUALLY p) UNTIL (GLOBALLY q)"
            "(GLOBALLY p) UNTIL q"
            "NOT (((EVENTUALLY (EVENTUALLY p)) IMPLIES (EVENTUALLY p)) AND ((EVENTUALLY p) IMPLIES (EVENTUALLY (EVENTUALLY p))))"
            #_"NOT (((EVENTUALLY (EVENTUALLY p)) AND (EVENTUALLY p)) OR ((NOT (EVENTUALLY (EVENTUALLY p))) AND (NOT (EVENTUALLY p))))"])

#_(map #(properties (automaton (parser/tree->tree (parser/parse-ltl %)))) tests)

(def au (automaton (parser/tree->tree (parser/parse-ltl "GLOBALLY ((NOT p) IMPLIES (NEXT p))"))))
(def au (automaton (parser/tree->tree (parser/parse-ltl "a UNTIL b"))))
(def au (automaton (parser/tree->tree (parser/parse-ltl "(EVENTUALLY a) IMPLIES (EVENTUALLY b)"))))

(defn advance [{:keys [accepted? valid? transitions f q i run l states] :as au} x-i]
  (if valid?
    (let [new-run (conj run x-i)
          targets (if (empty? run)
                    (into #{} (map :name i))
                    (into #{} (mapcat #(get transitions %) states)))
          new-states (into #{} (filter #(contains? (get l %) x-i) targets))
          new-valid? (not (empty? new-states))
          fn-and (fn [p n] (and p n))
          acc? (some true?
                     (map (fn [x]
                            (reduce fn-and true (map #(contains? % x) f)))
                          new-states))
          new-accepted? (conj accepted? acc?)]
      (merge au {:run new-run :states new-states
                 :valid? new-valid? :accepted? new-accepted?}))
    (update au :run conj x-i)))

(advance (advance (advance (advance (advance (advance (advance au #{}) #{"p"}) #{}) #{"p"}) #{}) #{}) #{"p"})
(advance (advance au #{"a"}) #{})
(advance (advance (advance (advance au #{"a"}) #{"a"}) #{"b"}) #{})
(advance (advance (advance (advance au #{[:N "a"]}) #{}) #{}) #{[:N "b"]})
(advance (advance (advance (advance (advance (advance au #{}) #{}) #{}) #{"a"}) #{}) #{"b"})

((fn [] au))
