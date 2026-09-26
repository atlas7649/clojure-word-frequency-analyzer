(ns analyzer.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [analyzer.core :refer [tokenize frequency-analysis vocabulary-size generate-ngrams sorted-frequencies most-common relative-frequencies word-length-distribution average-word-length text-summary keyword-density text-readability-score text-complexity-metrics batch-frequency-analysis jaccard-similarity calculate-idf tf-idf-analysis clean-text lexical-diversity global-ngram-analysis]]))

(deftest test-tokenize
  (testing "Basic tokenization"
    (is (= ["hello" "world"] (tokenize "Hello world!"))))
  (testing "Whitespace handling"
    (is (= ["foo" "bar"] (tokenize "  foo   bar  ")))))

(deftest test-clean-text
  (testing "Default cleaning"
    (is (= "Hello world " (clean-text "Hello world!"))))
  (testing "Custom cleaning pattern"
    (is (= "Hello world!" (clean-text "Hello world!" :pattern #"[0-9]")))))

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

(deftest test-tfidf
  (testing "IDF calculation"
    (let [docs ["apple banana" "apple cherry" "banana date"]
          idf (calculate-idf docs :stop-words #{})]
      ;; apple appears in 2/3 docs: log(3/2)
      ;; banana appears in 2/3 docs: log(3/2)
      ;; cherry appears in 1/3 docs: log(3/1)
      ;; date appears in 1/3 docs: log(3/1)
      (is (= (Math/log 1.5) (get idf "apple")))
      (is (= (Math/log 3.0) (get idf "cherry")))))
  (testing "TF-IDF Analysis"
    (let [docs-map {"d1" "apple apple banana" "d2" "apple cherry"}
          tfidf (tf-idf-analysis docs-map :stop-words #{})]
      ;; d1 TF: apple=2/3, banana=1/3
      ;; d2 TF: apple=1/2, cherry=1/2
      ;; IDF: apple=log(2/2)=0, banana=log(2/1)=log 2, cherry=log(2/1)=log 2
      (is (= 0.0 (get-in tfidf ["d1" "apple"])))
      (is (= (* (/ 1 3) (Math/log 2)) (get-in tfidf ["d1" "banana"])))
      (is (= (* (/ 1 2) (Math/log 2)) (get-in tfidf ["d2" "cherry"]))))))

(deftest test-lexical-diversity
  (testing "Lexical diversity calculation"
    (let [text "apple banana apple orange"]
      ;; Tokens: [apple banana apple orange] (4)
      ;; Unique: {apple banana orange} (3)
      ;; TTR: 3/4 = 0.75
      (is (= 0.75 (lexical-diversity text :stop-words #{})))))
  (testing "Lexical diversity with empty text"
    (is (= 0.0 (lexical-diversity "")))))

(deftest test-global-ngram-analysis
  (testing "Global n-gram frequencies"
    (let [docs {"d1" "i love clojure" "d2" "i love coding"}
          ngrams (global-ngram-analysis docs 2 :stop-words #{})]
      (is (= 2 (get ngrams ["i" "love"])))
      (is (= 1 (get ngrams ["love" "clojure"])))
      (is (= 1 (get ngrams ["love" "coding"]))))))