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

(defn vocabulary-size
  "Calculate the number of unique words in the text, optionally filtering stop words."
  [text & {:keys [stop-words] :or {stop-words default-stop-words}}]
  (count (set (->> (tokenize text)
                   (remove #(contains? stop-words %))))))

(defn sorted-frequencies
  "Return frequencies sorted by value in descending order, optionally filtering by a minimum count."
  [freq-map & {:keys [min-count] :or {min-count 0}}]
  (->> freq-map
       (remove (fn [[_ count]] (< count min-count)))
       (sort-by (fn [[_ count]] count))
       (reverse)))

(defn most-common
  "Return the top N most frequent items from a frequency map."
  [freq-map n]
  (take n (sorted-frequencies freq-map)))

(defn relative-frequencies
  "Calculate the relative frequency (percentage) of each word in a frequency map."
  [freq-map]
  (let [total (reduce + (vals freq-map))]
    (reduce-kv (fn [m k v] (assoc m k (/ v total)]) {} freq-map)))

(defn generate-ngrams
  "Generate n-grams from the provided text, optionally filtering stop words."
  [text n & {:keys [stop-words] :or {stop-words nil}}]
  (let [words (tokenize text)
        filtered-words (if stop-words (remove #(contains? stop-words %) words) words)]
    (->> filtered-words
         (partition n 1)
         (frequencies))))

(defn word-length-distribution
  "Calculate the frequency of word lengths in the text, optionally filtering stop words."
  [text & {:keys [stop-words] :or {stop-words default-stop-words}}]
  (->> (tokenize text)
       (remove #(contains? stop-words %))
       (map count)
       (frequencies)))

(defn average-word-length
  "Calculate the average length of words in the text, optionally filtering stop words."
  [text & {:keys [stop-words] :or {stop-words default-stop-words}}]
  (let [words (->> (tokenize text)
                    (remove #(contains? stop-words %)))]
    (if (empty? words)
      0
      (/ (reduce + (map count words)) (count words)))))

(defn text-summary
  "Return a summary containing vocabulary size and the top N most frequent words."
  [text n & {:keys [stop-words] :or {stop-words default-stop-words}}]
  (let [freqs (frequency-analysis text :stop-words stop-words)]
    {:vocabulary-size (vocabulary-size text :stop-words stop-words)
     :top-words (most-common freqs n)}))

(defn analyze-file
  "Read a file and return a map of word frequencies."
  [file-path]
  (let [content (slurp file-path)]
    (frequency-analysis content)))