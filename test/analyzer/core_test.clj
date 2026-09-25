(ns analyzer.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [analyzer.core :refer [tokenize frequency-analysis generate-ngrams sorted-frequencies most-common relative-frequencies]]))

(deftest test-tokenize
  (testing "Basic tokenization"
    (is (= ["hello" "world"] (tokenize "Hello world!"))))
  (testing "Whitespace handling"
    (is (= ["foo" "bar"] (tokenize "  foo   bar  ")))))

(deftest test-frequency-analysis
  (testing "Word counting with default stop-words"
    (let [text "The quick brown fox jumps over the lazy dog"]
      (is (= 1 ((frequency-analysis text) "quick")))
      (is (nil? ((frequency-analysis text) "the"))))))

(deftest test-sorted-frequencies
  (testing "Sorting and filtering frequencies"
    (let [freqs {"apple" 1 "banana" 3 "cherry" 2}]
      (is (= [["banana" 3] ["cherry" 2] ["apple" 1]] (sorted-frequencies freqs)))
      (is (= [["banana" 3] ["cherry" 2]] (sorted-frequencies freqs :min-count 2))))))

(deftest test-most-common
  (testing "Extracting top N items"
    (let [freqs {"apple" 1 "banana" 3 "cherry" 2}]
      (is (= [["banana" 3] ["cherry" 2]] (most-common freqs 2)))
      (is (= [["banana" 3]] (most-common freqs 1))))))

(deftest test-relative-frequencies
  (testing "Calculating relative frequencies"
    (let [freqs {"apple" 1 "banana" 3}]
      (is (= {"apple" (1/4) "banana" (3/4)} (relative-frequencies freqs))))))

(deftest test-ngrams
  (testing "Bigram generation"
    (let [text "I love Clojure I love coding"]
      (is (= 2 ((generate-ngrams text 2) ["i" "love"]))))))