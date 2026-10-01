# Clojure Word Frequency Analyzer

A small utility to analyze word and n-gram frequencies in text documents.

## Usage

```clojure
(require '[analyzer.core :as analyzer])

(analyzer/frequency-analysis "This is a test. This is only a test.")
;; => {"test" 2, "only" 1}

(analyzer/generate-ngrams "The quick brown fox jumps over the lazy dog" 2)
;; => {["the" "quick"] 1, ["quick" "brown"] 1, ...}

(analyzer/shannon-entropy "Apple banana apple orange")
;; => Calculates distribution entropy

(analyzer/cosine-similarity "apple apple banana" "apple banana banana")
;; => 0.8

(analyzer/gunning-fog-index "The quick brown fox jumps over the lazy dog.")
;; => Calculates a standard readability index

(analyzer/kullback-leibler-divergence "apple apple banana" "apple banana banana")
;; => Calculates information divergence between distributions

(analyzer/text-similarity-report "apple banana" "apple cherry")
;; => {:jaccard 0.33, :cosine 0.5, :manhattan 2, :euclidean 1.41, :hamming 2, :canberra 1.0, :bray-curtis 0.5}

(analyzer/dominant-ngram "apple banana apple banana cherry" 2)
;; => [["apple" "banana"] 2]

(analyzer/herdan-vocabulary "apple banana apple banana cherry date" 3)
;; => [0.66, 0.66, 1.0, 1.0]

(analyzer/most-significant-words "apple apple banana cherry cherry cherry" 2)
;; => [["cherry" 18] ["apple" 10]]
```