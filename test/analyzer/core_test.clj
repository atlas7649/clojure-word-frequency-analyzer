(ns analyzer.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [analyzer.core :refer [tokenize frequency-analysis vocabulary-size generate-ngrams sorted-frequencies most-common relative-frequencies word-length-distribution average-word-length text-summary keyword-density text-readability-score text-complexity-metrics batch-frequency-analysis jaccard-similarity]]))

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

(deftest test-vocabulary-size
  (testing "Vocabulary count with default stop-words"
    (let [text "The quick brown fox jumps over the lazy dog"]
      (is (= 7 (vocabulary-size text)))))
  (testing "Vocabulary count with custom stop-words"
    (let [text "apple banana apple orange"
          stop-words #{"orange"}]
      (is (= 2 (vocabulary-size text :stop-words stop-words))))))

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
      (is (= 2 ((generate-ngrams text 2) ["i" "love"])))))
  (testing "N-gram generation with stop-words"
    (let [text "The quick brown fox jumps over the lazy dog"
          stop-words #{"the" "over"}]
      (is (= 1 ((generate-ngrams text 2 :stop-words stop-words) ["quick" "brown"])))
      (is (nil? ((generate-ngrams text 2 :stop-words stop-words) ["the" "quick"]))))))

(deftest test-word-length-distribution
  (testing "Length distribution with default stop-words"
    (let [text "The quick brown fox"]
      ;; "the" is a stopword. "quick" (5), "brown" (5), "fox" (3)
      (is (= {5 2 3 1} (word-length-distribution text)))))
  (testing "Length distribution with custom stop-words"
    (let [text "apple banana cherry"
          stop-words #{"apple"}]
      ;; "banana" (6), "cherry" (6)
      (is (= {6 2} (word-length-distribution text :stop-words stop-words))))))

(deftest test-average-word-length
  (testing "Average length with default stop-words"
    (let [text "The quick brown fox"]
      ;; "quick"(5) + "brown"(5) + "fox"(3) = 13 / 3
      (is (= (/ 13 3) (average-word-length text)))))
  (testing "Average length with empty result"
    (is (= 0 (average-word-length "the the the" :stop-words #{"the"})))))

(deftest test-text-summary
  (testing "Text summary generation"
    (let [text "Apple banana apple orange cherry apple banana"
          stop-words #{"orange"}]
      (let [summary (text-summary text 2 :stop-words stop-words)]
        (is (= 3 (:vocabulary-size summary)))
        (is (= [["apple" 3] ["banana" 2]] (:top-words summary)))))))

(deftest test-keyword-density
  (testing "Keyword density calculation"
    (let [text "Clojure is a functional language. Clojure is powerful."
          keywords ["Clojure" "functional"]
      (is (= {"Clojure" (/ 2 8) "functional" (/ 1 8)} (keyword-density text keywords)))))
  (testing "Density with missing keywords"
    (let [text "Hello world"
          keywords ["missing"]
      (is (= {"missing" 0.0} (keyword-density text keywords)))))
  (testing "Density with empty text"
    (is (= {"test" 0.0} (keyword-density "" ["test"])))))

(deftest test-readability-score
  (testing "Readability score calculation"
    (let [text "This is a simple sentence. This is another one."]
      ;; Words: [this is a simple sentence this is another one] (9 words)
      ;; Sentences: 2
      ;; avg-sentence-length: 9 / 2 = 4.5
      ;; avg-word-length: (4+2+1+6+8+4+2+7+3) / 9 = 37 / 9 ≈ 4.11
      ;; Score: 0.4 * 4.5 + 0.6 * (37/9) = 1.8 + 22.2/9 = 1.8 + 2.466 = 4.266
      (is (> (text-readability-score text) 4.0))))
  (testing "Readability score with empty text"
    (is (= 0.0 (text-readability-score "")))))

(deftest test-text-complexity-metrics
  (testing "Complexity metrics aggregation"
    (let [text "The quick brown fox jumps over the lazy dog."]
      (let [metrics (text-complexity-metrics text)]
        (is (number? (:readability-score metrics)))
        (is (= 7 (:vocabulary-size metrics)))
        (is (= (/ 13 3) (:average-word-length metrics))))))
  (testing "Complexity metrics with empty text"
    (let [metrics (text-complexity-metrics "")]
      (is (= 0.0 (:readability-score metrics)))
      (is (= 0 (:vocabulary-size metrics)))
      (is (= 0 (:average-word-length metrics))))))

(deftest test-batch-analysis
  (testing "Analyzing multiple texts"
    (let [texts {"doc1" "apple banana apple" "doc2" "banana cherry"}]
      (let [results (batch-frequency-analysis texts :stop-words #{})]
        (is (= 2 ((get results "doc1") "apple")))
        (is (= 1 ((get results "doc2") "cherry")))))))

(deftest test-jaccard-similarity
  (testing "Similarity between identical texts"
    (is (= 1.0 (jaccard-similarity "the quick brown fox" "the quick brown fox"))))
  (testing "Similarity between disjoint texts"
    (is (= 0.0 (jaccard-similarity "apple banana" "cherry date"))))
  (testing "Partial similarity"
    (let [t1 "apple banana cherry"
          t2 "banana cherry date"]
      ;; Sets: {apple banana cherry}, {banana cherry date}
      ;; Intersection: {banana cherry} (2)
      ;; Union: {apple banana cherry date} (4)
      (is (= 0.5 (jaccard-similarity t1 t2 :stop-words #{}))))))