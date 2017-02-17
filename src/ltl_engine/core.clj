(ns ltl-engine.core
  (:require [clojure.set :as set]
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
  (let [perms (combo/subsets (into [] cl-set))]
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
        true #_(reduce
                r-and true
                (map #(and (contains? m (second %))
                           (contains? m %))
                     (filter #(= :U (first %)) m-prima)))
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
        right-2 true #_(reduce
                        r-and true
                        (map #(and (contains? m (second %))
                                   (contains? m %))
                             (filter #(= :U (first %)) m-prima)))
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

(defn f-set [ltl]
  (let [cl-s (cl-f ltl)
        cs-s (cs cl-s)]
    (into #{}
          (map
           #(into #{} (filter (fn [m-s]
                                (or
                                 (contains? m-s (nth % 2))
                                 (contains? m-s [:N %])))
                              (map (fn [x] (into #{} x)) cs-s)))
           (filter #(= :U (first %)) cl-s)))))

(defn gba [ltl]
  (let [cl-s (cl-f ltl)
        cs-s (cs cl-s)
        a1 (automata-1 ltl)
        a2 (automata-2 ltl)
        ap (filter string? cl-s)
        ap-s (into #{} ap)
        ap-2 (combo/subsets ap)]
    {:state-space (set/union #{ltl} cs-s)
     :propositions ap-2
     :transitions (into #{} (set/union a1 a2))
     :states #{ltl}
     :f (f-set ltl)}))

(defn transitions [gba propositions]
  (let [states (:states gba)
        p-s (into #{} propositions)
        transitions (into #{}
                          (mapcat
                           #(filter
                             (fn [x]
                               (and (= (:m x) %)
                                    (= (:a x) p-s)))
                             (:transitions gba))
                           states))]
    transitions))

(defn advance [gba propositions]
  (let [destinations (map :m-prima (transitions gba propositions))]
    (assoc gba :states (into #{} destinations))))

#_(let [g (gba [:U "a" "b"])]
  (advance (advance (advance g ["a"]) ["b"]) []))
