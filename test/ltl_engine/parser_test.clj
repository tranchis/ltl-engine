(ns ltl-engine.parser-test
  (:require [ltl-engine.parser :as sut]
            [midje.sweet :as midje]))

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

