(ns ltl-engine.core
  (:require [ltl-engine.parser :as parser]
            [clojure.set :as set]
            [clojure.math.combinatorics :as combo]))

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
    (if (or (string? ltl) (and (= (first ltl) :N) (string? (second ltl))))
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

(defn proposition? [formula]
  (or (string? formula) (= [:TRUE] formula) (= [:FALSE] formula)
      (and (= :N (first formula)) (string? (second formula)))))

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
  (let [perms (map #(into #{} %) (combo/subsets (into [] cl-set)))]
    (into #{} (filter #(is-maximal? cl-set %) perms))))

(defn positives [ap-s m]
  (into #{} (filter #(not (contains? m [:N %])) ap-s)))

(defn fulfills-next? [m m-prima]
  (let [r-and (fn [p n] (and p n))
        right true #_(reduce r-and true (map #(contains? m [:X %]) m-prima))
        left (reduce r-and true (map #(contains? m-prima (second %))
                                     (filter #(= :X (first %)) m)))]
    (and right left)))

(defn fulfills-until? [m m-prima]
  (let [r-and (fn [p n] (and p n))
        right-1 true #_(reduce
                 r-and true
                 (map #(or
                        (= 0 (count (filter (fn [x] (= :U (first x))) m)))
                        (> (count (filter (fn [x]
                                            (and (= :U (first x)) (= % (nth x 2))))
                                          m))
                           0))
                      (remove #(= :U (first %)) m)))
        right-2
        (reduce
         r-and true
         (map #(let [r (second %)]
                 (and
                  (not (contains? m (nth r 2)))
                  (not (and (contains? m (second r))
                            (contains? m-prima r)))))
              (filter #(and (= :N (first %))
                            (= :U (first (second %))))
                      m)))
        left (reduce
              r-and true
              (map #(or (contains? m (nth % 2))
                        (and (contains? m (second %))
                             (contains? m-prima %)))
                   (filter #(= :U (first %)) m)))]
    (and right-1 right-2 left)))

(defn fulfills-release? [m m-prima]
  (let [r-and (fn [p n] (and p n))
        right-1 true #_(reduce
                        r-and true
                        (map #(> (count (filter (fn [x]
                                                  (and (= :U (first x)) (= % (nth x 2))))
                                                m))
                                 0)
                             (remove #(= :U (first %)) m)))
        right-2 (reduce
                 r-and true
                 (map #(let [r (second %)]
                         (and
                          (not (contains? m [:A (second r) (nth r 2)]))
                          (not (and (contains? m (nth r 2))
                                    (contains? m-prima r)))))
                      (filter #(and (= :N (first %))
                                    (= :R (first (second %))))
                              m)))
        left (reduce
              r-and true
              (map #(or (contains? m [:A (second %) (nth % 2)])
                        (and (contains? m (nth % 2))
                             (contains? m-prima %)))
                   (filter #(= :R (first %)) m)))]
    (and right-1 right-2 left)))

(defn subset-ap? [ap-s m-prima a]
  (let [intersect (set/intersection m-prima ap-s)
        subset-a? (set/subset? intersect a)
        positives (positives ap-s m-prima)
        subset-p? (set/subset? a positives)]
    (and subset-a? subset-p?)))

(defn valid-trans? [ap-s {:keys [m a m-prima]}]
  (let [ss? (subset-ap? ap-s m-prima a)
        iff-x? (fulfills-next? m m-prima)
        iff-u? (fulfills-until? m m-prima)
        iff-r? (fulfills-release? m m-prima)]
    (and ss? iff-x? iff-u? iff-r?)))

(defn automata-1 [ltl]
  (let [cl-s (cl-f ltl)
        cs-s (cs cl-s)
        ap (filter string? cl-s)
        ap-s (into #{} ap)
        ap-2 (combo/subsets ap)
        m-s (mapcat #(vector % (reverse %)) (combo/combinations cs-s 2))
        m-s (concat m-s (map #(vector % %) cs-s))
        mam-s (map #(hash-map :m (into #{} (first (first %)))
                              :a (into #{} (second %))
                              :m-prima (into #{} (second (first %))))
                   (combo/cartesian-product m-s ap-2))
        valid-trans (filter #(valid-trans? ap-s %) mam-s)]
    valid-trans))

(defn automata-2 [ltl]
  (let [init ltl
        cl-s (cl-f ltl)
        cs-s (cs cl-s)
        ap (filter string? cl-s)
        ap-s (into #{} ap)
        ap-2 (combo/subsets ap)
        mam-s (map #(hash-map :m init
                              :a (into #{} (second %))
                              :m-prima (into #{} (first %)))
                   (combo/cartesian-product cs-s ap-2))
        valid-trans (filter #(and
                              (contains? (:m-prima %) ltl)
                              (subset-ap? ap-s (:m-prima %) (:a %))) mam-s)]
    valid-trans))

(defn f-set [cl-s cs-s]
  (let [us (filter #(= :U (first %)) cl-s)]
    (if (> (count us) 0)
      (into []
            (map
             #(into #{} (filter (fn [m-s]
                                  (or
                                   (contains? m-s (nth % 2))
                                   (not (contains? m-s %))))
                                (map (fn [x] (into #{} x)) cs-s)))
             us))
      [cs-s])))

(defn gba [ltl]
  (let [cl-s (cl-f ltl)
        cs-s (cs cl-s)
        a1 (automata-1 ltl)
        #_#_a2 (automata-2 ltl)
        ap (filter string? cl-s)
        ap-s (into #{} ap)
        ap-2 (combo/subsets ap)
        q-0 (into #{} (filter #(contains? % ltl) cs-s))]
    {:sigma ap-2
     :q cs-s
     :q-0 q-0
     :states q-0
     :run []
     :delta a1 #_(into #{} (set/union a1 a2))
     :f (f-set cl-s cs-s)}))

(defn transitions [gba propositions]
  (let [states (:states gba)
        p-s (into #{} propositions)
        transitions (into #{}
                          (mapcat
                           #(filter
                             (fn [x]
                               (and (= (:m x) %)
                                    (= (:a x) p-s)))
                             (:delta gba))
                           states))]
    transitions))

(defn advance [gba propositions]
  (let [destinations (map :m-prima (transitions gba propositions))
        destinations-s (into #{} destinations)]
    (assoc (update-in gba [:run] conj destinations-s)
           :states destinations-s)))

#_(let [g (gba (parser/tree->tree (parser/parse-ltl "GLOBALLY ((NOT p) IMPLIES (NEXT p))")))]
  (advance (advance (advance (advance g ["p"]) []) []) ["p"]))

(let [g (gba (parser/tree->tree (parser/parse-ltl "GLOBALLY ((NOT p) IMPLIES (NEXT p))")))]
  (advance g ["p"]))

(let [g (gba (parser/tree->tree (parser/parse-ltl "a UNTIL b")))]
  (advance (advance (advance (advance (advance (advance g ["a"]) ["a"]) ["a"]) []) []) []))

#_(let [g (gba (parser/tree->tree (parser/parse-ltl "a UNTIL (b UNTIL a)")))]
    (let [gg (advance (advance (advance (advance g ["a"]) ["b"]) ["a"]) [])
          states (:states gg)]
      (map #(map (fn [x] (contains? x %)) (:f gg)) states)))
