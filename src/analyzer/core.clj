(ns analyzer.core
  (:require [clojure.string :as str]))

(def default-stop-words
  #{"the" "and" "a" "an" "of" "to" "in" "is" "it" "that" "as" "for" "was" "with" "on"})

(defn tokenize
  "Split text into a sequence of lowercase words, removing non-alphanumeric characters."
  [text]
  (->> text
       (str/lower-case)
       (str/split #\s+)
       (map #(str/replace % #[^\W] ""))
       (remove empty?)))

(defn frequency-analysis
  "Calculate word frequencies, optionally filtering out stop words."
  [text & {:keys [stop-words] :or {stop-words default-stop-words}}]
  (->> (tokenize text)
       (remove #(contains? stop-words %))
       (frequencies)))

(defn generate-ngrams
  "Generate n-grams from the provided text."
  [text n]
  (let [words (tokenize text)]
    (->> words
         (partition n 1)
         (frequencies))))

(defn analyze-file
  "Read a file and return a map of word frequencies."
  [file-path]
  (let [content (slurp file-path)]
    (frequency-analysis content)))