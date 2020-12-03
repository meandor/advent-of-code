(ns day2.core-test
  (:require [clojure.test :refer :all]
            [day2.core :as core]))

(deftest should-parse-line
  (testing "same number of min max occurrence"
    (is (= {:password     "foo"
            :first-index  1
            :second-index 1
            :letter       "f"}
           (core/parse-line "1-1 f: foo"))))

  (testing "different number of min max occurrence"
    (is (= {:password     "foo"
            :first-index  1
            :second-index 3
            :letter       "o"}
           (core/parse-line "1-3 o: foo")))))

(deftest should-count-letters
  (testing "one occurrence"
    (is (= 1 (core/count-letter-occurrence "f" "foo"))))

  (testing "no occurrence"
    (is (= 0 (core/count-letter-occurrence "k" "foo"))))

  (testing "n occurrence"
    (is (= 3 (core/count-letter-occurrence "b" "foobbb")))))

(deftest should-check-is-valid-sled-password?
  (with-redefs [core/count-letter-occurrence (constantly 1)]
    (testing "valid password with min=occurrence=max"
      (is (= true (core/valid-sled-password? {:password     "foo"
                                              :first-index  1
                                              :second-index 1
                                              :letter       "f"}))))

    (testing "valid password with min<=occurrence<max"
      (is (= true (core/valid-sled-password? {:password     "foo"
                                              :first-index  1
                                              :second-index 2
                                              :letter       "f"})))))

  (with-redefs [core/count-letter-occurrence (constantly 2)]
    (testing "valid password with min<occurrence<=max"
      (is (= true (core/valid-sled-password? {:password     "foo"
                                              :first-index  1
                                              :second-index 2
                                              :letter       "o"}))))
    (testing "invalid password"
      (is (= false (core/valid-sled-password? {:password     "foo"
                                               :first-index  3
                                               :second-index 4
                                               :letter       "o"}))))))

(deftest should-check-is-valid-toboggan-password?
  (testing "false password with same index"
    (is (= false (core/valid-toboggan-password? {:password     "fob"
                                                :first-index  2
                                                :second-index 2
                                                :letter       "o"}))))

  (testing "valid password with only first index"
    (is (= true (core/valid-toboggan-password? {:password     "fob"
                                                :first-index  1
                                                :second-index 2
                                                :letter       "f"}))))

  (testing "valid password with only second index"
    (is (= true (core/valid-toboggan-password? {:password     "fob"
                                                :first-index  1
                                                :second-index 2
                                                :letter       "o"}))))

  (testing "invalid password both indices are available"
    (is (= false (core/valid-toboggan-password? {:password     "foo"
                                                :first-index  2
                                                :second-index 3
                                                :letter       "o"}))))

  (testing "invalid password does not contain letter"
    (is (= false (core/valid-toboggan-password? {:password     "foo"
                                                :first-index  1
                                                :second-index 3
                                                :letter       "k"})))))
