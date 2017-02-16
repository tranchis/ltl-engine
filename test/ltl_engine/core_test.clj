(ns ltl-engine.core-test
  (:require [clojure.test :refer :all]
            [ltl-engine.core :refer :all]
            [midje.sweet :refer :all]))

(deftest ltl-parser-test
  (testing "Valid conversions"
    (fact (parse-ltl "a") => [:S [:T "a"]])
    (fact (parse-ltl "TRUE") => [:S [:TRUE]])
    (fact (parse-ltl "FALSE") => [:S [:FALSE]])
    (fact (parse-ltl "NOT a") => [:S [:N [:S [:T "a"]]]])
    (fact (parse-ltl "(a AND b)") => [:S [:S [:A [:S [:T "a"]] [:S [:T "b"]]]]])
    (fact (parse-ltl "a IMPLIES b") => [:S [:I [:S [:T "a"]] [:S [:T "b"]]]])
    (fact (parse-ltl "(a AND b AND c AND d)") =>
          [:S [:S [:A [:S [:T "a"]]
                   [:S [:A [:S [:T "b"]]
                        [:S [:A [:S [:T "c"]]
                             [:S [:T "d"]]]]]]]]])
    (fact (parse-ltl "NEXT a") => [:S [:X [:S [:T "a"]]]])
    (fact (parse-ltl "EVENTUALLY a") => [:S [:F [:S [:T "a"]]]])
    (fact (parse-ltl "GLOBALLY a") => [:S [:G [:S [:T "a"]]]])
    (fact (parse-ltl "((a AND b) OR (c AND d))") =>
          [:S [:S [:O [:S [:S [:A [:S [:T "a"]]
                               [:S [:T "b"]]]]]
                   [:S [:S [:A [:S [:T "c"]]
                            [:S [:T "d"]]]]]]]])
    (fact (parse-ltl "((a AND TRUE) OR (FALSE AND d))") =>
          [:S [:S [:O [:S [:S [:A [:S [:T "a"]]
                               [:S [:TRUE]]]]]
                   [:S [:S [:A [:S [:FALSE]]
                            [:S [:T "d"]]]]]]]]))
  (testing "Invalid conversions"
    (fact (class (parse-ltl "X a")) => instaparse.gll.Failure)))

(deftest tree->tree-test
  (testing "Correct pruning"
    (fact (tree->tree [:S]) => [])
    (fact (tree->tree [:S [:T "a"]]) => "a")
    (fact (tree->tree [:S [:N [:S [:T "a"]]]]) => [:N "a"])
    (fact (tree->tree [:S [:S [:A [:S [:T "a"]] [:S [:T "b"]]]]]) => [:A "a" "b"])
    (fact (tree->tree [:S [:S [:A [:S [:T "a"]]
                               [:S [:A [:S [:T "b"]]
                                    [:S [:A [:S [:T "c"]]
                                         [:S [:T "d"]]]]]]]]]) =>
          [:A "a" [:A "b" [:A "c" "d"]]])
    (fact (tree->tree [:S [:S [:O [:S [:S [:A [:S [:T "a"]]
                                           [:S [:T "b"]]]]]
                               [:S [:S [:A [:S [:T "c"]]
                                        [:S [:T "d"]]]]]]]]) =>
          [:O [:A "a" "b"] [:A "c" "d"]])
    (fact (tree->tree [:S [:I [:S [:T "a"]] [:S [:T "b"]]]]) => [:O [:N "a"] "b"])
    (fact (tree->tree [:S [:F [:S [:T "a"]]]]) => [:U :TRUE "a"])
    (fact (tree->tree [:S [:G [:S [:T "a"]]]]) => [:R :FALSE "a"])
    (fact (tree->tree [:S [:N [:S [:G [:S [:T "a"]]]]]]) => [:U :TRUE [:N "a"]])))

(deftest negate-test
  (fact (negate [:TRUE]) => [:FALSE])
  (fact (negate [:FALSE]) => [:TRUE])
  (fact (negate []) => [:N []])
  (fact (negate "a") => [:N "a"])
  (fact (negate [:F "a"]) => [:N [:F "a"]])
  (fact (negate [:N "a"]) => "a"))

(deftest cl-test
  (fact (cl-f []) => #{[:TRUE] [:FALSE]})
  (fact (cl-f "a") => #{[:TRUE] [:FALSE] "a" [:N "a"]})
  (fact (cl-f [:N "a"]) => #{[:TRUE] [:FALSE] "a" [:N "a"]})
  (fact (cl-f [:X "a"]) => #{[:TRUE] [:FALSE] [:X "a"] [:N [:X "a"]] "a" [:N "a"]})
  (fact (cl-f [:A "a" "b"]) => #{[:TRUE] [:FALSE] [:A "a" "b"]
                                 [:N [:A "a" "b"]] "a" [:N "a"] "b" [:N "b"]})
  (fact (cl-f [:O [:A "a" "b"] [:A "c" "d"]]) => 
        #{[:TRUE] [:FALSE] [:O [:A "a" "b"] [:A "c" "d"]]
          [:N [:O [:A "a" "b"] [:A "c" "d"]]]
          [:A "a" "b"] [:N [:A "a" "b"]] [:A "c" "d"] [:N [:A "c" "d"]]
          "a" [:N "a"] "b" [:N "b"] "c" [:N "c"] "d" [:N "d"]}))

(deftest is-maximal-test
  (fact (is-maximal? #{} #{}) => false)
  (fact (is-maximal? #{[:TRUE]} #{[:TRUE]}) => true)
  (fact (is-maximal? #{[:TRUE] "a"} #{[:TRUE] "a"}) => true)
  (fact (is-maximal? #{[:TRUE] "a" [:N "a"]} #{[:TRUE] "a" [:N "a"]}) => false)
  (fact (is-maximal? #{[:TRUE] [:A "a" "b"]} #{[:TRUE] [:A "a" "b"]}) => false)
  (fact (is-maximal? #{[:TRUE] [:A "a" "b"] "a"} #{[:TRUE] [:A "a" "b"] "a"}) => false)
  (fact (is-maximal? #{[:TRUE] [:A "a" "b"] "a" "b"} #{[:TRUE] [:A "a" "b"] "a" "b"}) => true)
  (fact (is-maximal? #{[:TRUE] [:A "a" "b"] "a" "b"} #{[:TRUE] "a" "b"}) => false)
  (fact (is-maximal? #{[:TRUE] [:O "a" "b"]} #{[:TRUE] [:O "a" "b"]}) => false)
  (fact (is-maximal? #{[:TRUE] [:O "a" "b"] "a"} #{[:TRUE] [:O "a" "b"] "a"}) => true)
  (fact (is-maximal? #{[:TRUE] [:O "a" "b"] "a" "b"} #{[:TRUE] [:O "a" "b"] "a" "b"}) => true)
  (fact (is-maximal? #{[:TRUE] [:A "a" "b"] "a" "b"} #{[:TRUE] "a" "b"}) => false))

(deftest check-ors-test
  (fact (check-ors #{[:O "a" "b"] "a" "b"} #{"a"} "a") => false)
  (fact (check-ors #{[:O "a" "b"] "a" "b"} #{[:O "a" "b"] "a"} "a") => true)
  (fact (check-ors #{[:O "a" "b"] [:O "a" "c"] "a" "b" "c"} #{[:O "a" "b"] "a"} "a") => false)
  (fact (check-ors #{[:O "a" "b"] [:O "a" "c"] "a" "b" "c"} #{[:O "a" "b"] [:O "a" "c"] "a"} "a") => true)
  (fact (check-ors #{[:O "a" "b"] "a" "b"} #{"b"} "d") => true))

(deftest cs-test
  (fact (cs #{}) => '())
  (fact (into '() (cs #{[:TRUE] [:FALSE] [:O "a" "b"]
                        [:N [:O "a" "b"]] "a" [:N "a"] "b" [:N "b"]})) =>
        '(([:N "b"] [:N "a"] [:N [:O "a" "b"]] [:TRUE])
          ([:O "a" "b"] "a" "b" [:TRUE])
          ([:N "b"] [:O "a" "b"] "a" [:TRUE])
          ([:O "a" "b"] [:N "a"] "b" [:TRUE])))
  (fact (into '() (cs #{[:TRUE] [:FALSE] [:A "a" "b"]
                        [:N [:A "a" "b"]] "a" [:N "a"] "b" [:N "b"]})) =>
        '(([:N "b"] "a" [:N [:A "a" "b"]] [:TRUE])
          ([:N "b"] [:N "a"] [:N [:A "a" "b"]] [:TRUE])
          ([:N "a"] [:N [:A "a" "b"]] "b" [:TRUE])
          ([:A "a" "b"] "a" "b" [:TRUE]))))
