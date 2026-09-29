(ns analyzer.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [analyzer.core :refer [tokenize frequency-analysis vocabulary-size generate-ngrams sorted-frequencies most-common relative-frequencies word-length-distribution average-word-length text-summary keyword-density text-readability-score text-complexity-metrics batch-frequency-analysis jaccard-similarity cosine-similarity manhattan-distance calculate-idf tf-idf-analysis tf-idf-top-terms clean-text lexical-diversity global-ngram-analysis normalize-text add-stop-words remove-stop-words common-words-analysis document-frequency-mapping shannon-entropy zipfs-law-analysis gunning-fog-index word-cloud-data text-to-freq-map cluster-documents kullback-leibler-divergence]]))

(deftest test-tokenize
  (testing "Basic tokenization"
    (is (= ["hello" "world"] (tokenize "Hello world!"))))
  (testing "Whitespace handling"
    (is (= ["foo" "bar"] (tokenize "  foo   bar  ")))))

(deftest test-normalize-text
  (testing "Text normalization"
    (is (= "hello world" (normalize-text "  Hello   World  ")))))

(deftest test-stop-word-management
  (testing "Adding stop words"
    (let [stops #{"the"} 
          new-stops (add-stop-words stops ["and" "a"])]
      (is (= #{"the" "and" "a"} new-stops))))
  (testing "Removing stop words"
    (let [stops #{"the" "and"} 
          new-stops (remove-stop-words stops ["the"])]
      (is (= #{"and"} new-stops)))))

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

(deftest test-gunning-fog-index
  (testing "Gunning Fog Index calculation"
    (let [text "The quick brown fox jumps over the lazy dog. This is a sophisticated demonstration."]
      ;; Sentence 1: 9 words
      ;; Sentence 2: 5 words
      ;; Total words: 14
      ;; Total sentences: 2
      ;; Avg sentence length: 14 / 2 = 7.0
      ;; Complex words (>6 chars): [sophisticated, demonstration] (2 words)
      ;; Pct complex: (2/14)*100 ≈ 14.28%
      ;; Score: 0.4 * (7.0 + 14.28) = 0.4 * 21.28 ≈ 8.51
      (is (> (gunning-fog-index text) 8.0))))
  (testing "Gunning Fog Index with empty text"
    (is (= 0.0 (gunning-fog-index "")))))

(deftest test-text-complexity-metrics
  (testing "Complexity metrics aggregation"
    (let [text "The quick brown fox jumps over the lazy dog."]
      (let [metrics (text-complexity-metrics text)]
        (is (number? (:readability-score metrics)))
        (is (number? (:gunning-fog-index metrics)))
        (is (= 7 (:vocabulary-size metrics)))
        (is (= (/ 13 3) (:average-word-length metrics)))
        (is (number? (:entropy metrics))))))
  (testing "Complexity metrics with empty text"
    (let [metrics (text-complexity-metrics "")]
      (is (= 0.0 (:readability-score metrics)))
      (is (= 0.0 (:gunning-fog-index metrics)))
      (is (= 0 (:vocabulary-size metrics)))
      (is (= 0 (:average-word-length metrics)))
      (is (= 0.0 (:entropy metrics)))))))

(deftest test-batch-analysis
  (testing "Analyzing multiple texts"
    (let [texts {"doc1" "apple banana apple" "doc2" "banana cherry"}]
      (let [results (batch-frequency-analysis texts :stop-words #{})]
        (is (= 2 ((get results "doc1") "apple")))
        (is (= 1 ((get results "doc2") "cherry")))))))

(deftest test-similarity-metrics
  (testing "Jaccard similarity"
    (is (= 1.0 (jaccard-similarity "the quick brown fox" "the quick brown fox")))
    (is (= 0.0 (jaccard-similarity "apple banana" "cherry date"))))
  (testing "Cosine similarity"
    (let [t1 "apple apple banana"
          t2 "apple banana banana"]
      ;; f1: {apple 2, banana 1}, f2: {apple 1, banana 2}
      ;; dot: 2*1 + 1*2 = 4
      ;; mag1: sqrt(4+1)=sqrt(5), mag2: sqrt(1+4)=sqrt(5)
      ;; cos: 4 / (sqrt(5)*sqrt(5)) = 4/5 = 0.8
      (is (= 0.8 (cosine-similarity t1 t2 :stop-words #{})))))
  (testing "Manhattan distance"
    (let [t1 "apple apple banana"
          t2 "apple banana banana"]
      ;; f1: {apple 2, banana 1}, f2: {apple 1, banana 2}
      ;; dist: |2-1| + |1-2| = 1 + 1 = 2
      (is (= 2 (manhattan-distance t1 t2 :stop-words #{}))))))

(deftest test-kl-divergence
  (testing "KL Divergence identical distributions"
    (let [t1 "apple banana"
          t2 "apple banana"]
      (is (= 0.0 (kullback-leibler-divergence t1 t2 :stop-words #{})))))
  (testing "KL Divergence different distributions"
    (let [t1 "apple apple banana"
          t2 "apple banana banana"]
      ;; P: {apple 2/3, banana 1/3}, Q: {apple 1/3, banana 2/3}
      ;; KL = (2/3 * log( (2/3)/(1/3) )) + (1/3 * log( (1/3)/(2/3) ))
      ;; KL = (2/3 * log 2) + (1/3 * log 0.5) = (2/3 * log 2) - (1/3 * log 2) = (1/3 * log 2)
      (is (= (/ (Math/log 2) 3) (kullback-leibler-divergence t1 t2 :stop-words #{})))))
  (testing "KL Divergence with disjoint sets"
    (let [t1 "apple"
          t2 "banana"]
      ;; P: {apple 1.0}, Q: {banana 1.0}
      ;; KL = 1.0 * log(1.0 / epsilon)
      (is (> (kullback-leibler-divergence t1 t2 :stop-words #{}) 10.0))))))

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
      (is (= 0.0 (get-in tfidf ["d1" "apple")))
      (is (= (* (/ 1 3) (Math/log 2)) (get-in tfidf ["d1" "banana")))
      (is (= (* (/ 1 2) (Math/log 2)) (get-in tfidf ["d2" "cherry"])))))
  (testing "TF-IDF Top Terms"
    (let [docs-map {"d1" "apple apple banana" "d2" "apple cherry"}
          top-terms (tf-idf-top-terms docs-map 1 :stop-words #{})]
      ;; d1: banana is most important (apple is 0.0)
      ;; d2: cherry is most important (apple is 0.0)
      (is (= [["banana" (* (/ 1 3) (Math/log 2))]] (get top-terms "d1")))
      (is (= [["cherry" (* (/ 1 2) (Math/log 2))]] (get top-terms "d2")))))))

(deftest test-lexical-diversity
  (testing "Lexical diversity calculation"
    (let [text "apple banana apple orange"]
      ;; Tokens: [apple banana apple orange] (4)
      ;; Unique: {apple banana orange} (3)
      ;; TTR: 3/4 = 0.75
      (is (= 0.75 (lexical-diversity text :stop-words #{})))))
  (testing "Lexical diversity with empty text"
    (is (= 0.0 (lexical-diversity "")))))

(deftest test-shannon-entropy
  (testing "Entropy of uniform distribution"
    (let [text "apple banana"
          entropy (shannon-entropy text :stop-words #{})]
      ;; Probabilities: 0.5, 0.5
      ;; Entropy: 0.5*log(0.5) + 0.5*log(0.5) = log(0.5)
      (is (= (Math/log 0.5) entropy))))
  (testing "Entropy of single word"
    (is (= 0.0 (shannon-entropy "apple apple apple" :stop-words #{}))))
  (testing "Entropy of empty text"
    (is (= 0.0 (shannon-entropy "" :stop-words #{})))))

(deftest test-global-ngram-analysis
  (testing "Global n-gram frequencies"
    (let [docs {"d1" "i love clojure" "d2" "i love coding"}
          ngrams (global-ngram-analysis docs 2 :stop-words #{})]
      (is (= 2 (get ngrams ["i" "love")))
      (is (= 1 (get ngrams ["love" "clojure")))
      (is (= 1 (get ngrams ["love" "coding"]))))))

(deftest test-common-words
  (testing "Finding words common to all documents"
    (let [batch-freqs {"d1" {"apple" 1 "banana" 1} "d2" {"banana" 1 "cherry" 1}}]
      (is (= #{"banana"} (common-words-analysis batch-freqs)))))
  (testing "Common words with no overlap"
    (let [batch-freqs {"d1" {"apple" 1} "d2" {"banana" 1}}]
      (is (= #{} (common-words-analysis batch-freqs)))))
  (testing "Common words with empty batch"
    (is (= #{} (common-words-analysis {})))))

(deftest test-document-frequency
  (testing "Calculating document frequency"
    (let [batch-freqs {"d1" {"apple" 1 "banana" 1} "d2" {"banana" 1 "cherry" 1}}]
      (is (= {"apple" 1 "banana" 2 "cherry" 1} (document-frequency-mapping batch-freqs))))))

(deftest test-zipfs-law
  (testing "Zipf's law distribution"
    (let [text "apple apple apple banana banana cherry"
          analysis (zipfs-law-analysis text 3 :stop-words #{})]
      ;; Rank 1: apple (3), predicted: 3/1 = 3
      ;; Rank 2: banana (2), predicted: 3/2 = 1.5
      ;; Rank 3: cherry (1), predicted: 3/3 = 1
      (is (= [1 3 3.0] (first analysis)))
      (is (= [2 2 1.5] (second analysis)))
      (is (= [3 1 1.0] (third analysis)))))
  (testing "Zipf's law with empty text"
    (is (= [] (zipfs-law-analysis "" 3 :stop-words #{})))))

(deftest test-word-cloud-data
  (testing "Generating word cloud data"
    (let [text "apple banana apple orange apple banana"
          data (word-cloud-data text 2 :stop-words #{})]
      (is (= [{:text "apple" :value 3} {:text "banana" :value 2}] data)))))

(deftest test-text-to-freq-map
  (testing "Direct text to frequency map conversion"
    (let [text "apple banana apple"
          freqs (text-to-freq-map text #{})]
      (is (= {"apple" 2 "banana" 1} freqs)))))

(deftest test-cluster-documents
  (testing "Basic clustering"
    (let [docs {"d1" "apple apple banana" "d2" "apple banana banana" "d3" "cherry date"}]
      (let [clusters (cluster-documents docs 0.5 :stop-words #{})]
        (is (= 2 (count clusters)))
        (is (some (fn [c] (and (contains? (set c) "d1") (contains? (set c) "d2"))) clusters))
        (is (some (fn [c] (contains? (set c) "d3")) clusters))))))