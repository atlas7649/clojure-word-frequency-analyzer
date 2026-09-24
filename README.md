# Clojure Word Frequency Analyzer

A small utility to analyze word and n-gram frequencies in text documents.

## Usage

```clojure
(require '[analyzer.core :as analyzer])

(analyzer/frequency-analysis "This is a test. This is only a test.")
;; => {"test" 2, "only" 1}

(analyzer/generate-ngrams "The quick brown fox jumps over the lazy dog" 2)
;; => {["the" "quick"] 1, ["quick" "brown"] 1, ...}
```