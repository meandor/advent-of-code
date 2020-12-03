(ns day2.core-test
  (:require [clojure.test :refer :all]
            [day2.core :as core]))

(deftest should-parse-line
  (testing "same number of min max occurrence"
    (is (= {:password       "foo"
            :min-occurrence 1
            :max-occurrence 1
            :letter         "f"}
           (core/parse-line "1-1 f: foo"))))

  (testing "different number of min max occurrence"
    (is (= {:password       "foo"
            :min-occurrence 1
            :max-occurrence 3
            :letter         "o"}
           (core/parse-line "1-3 o: foo")))))
