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

(deftest should-count-letters
  (testing "one occurrence"
    (is (= 1 (core/count-letter-occurrence "f" "foo"))))

  (testing "no occurrence"
    (is (= 0 (core/count-letter-occurrence "k" "foo"))))

  (testing "n occurrence"
    (is (= 3 (core/count-letter-occurrence "b" "foobbb")))))

(deftest should-check-is-valid-password?
  (with-redefs [core/count-letter-occurrence (constantly 1)]
    (testing "valid password with min=occurrence=max"
      (is (= true (core/valid-sled-password? {:password  "foo"
                                         :min-occurrence 1
                                         :max-occurrence 1
                                         :letter         "f"}))))

    (testing "valid password with min<=occurrence<max"
      (is (= true (core/valid-sled-password? {:password  "foo"
                                         :min-occurrence 1
                                         :max-occurrence 2
                                         :letter         "f"})))))

  (with-redefs [core/count-letter-occurrence (constantly 2)]
    (testing "valid password with min<occurrence<=max"
      (is (= true (core/valid-sled-password? {:password  "foo"
                                         :min-occurrence 1
                                         :max-occurrence 2
                                         :letter         "o"}))))
    (testing "invalid password"
      (is (= false (core/valid-sled-password? {:password  "foo"
                                          :min-occurrence 3
                                          :max-occurrence 4
                                          :letter         "o"}))))))
