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

(defn keyword-density
  "Calculate the density of specific keywords in the text relative to the total word count."
  [text keywords]
  (let [words (tokenize text)
        total (count words)]
    (if (zero? total)
      (reduce (fn [m k] (assoc m k 0.0)) {} keywords)
      (let [freqs (frequencies words)]
        (reduce (fn [m k] 
                  (let [count (get freqs (str/lower-case k) 0)]
                    (assoc m k (/ count total))))
                {} 
                keywords)))))

(defn analyze-file
  "Read a file and return a map of word frequencies."
  [file-path]
  (let [content (slurp file-path)]
    (frequency-analysis content)))

(defn text-readability-score
  "Calculate a basic readability score based on average word length and average sentence length.
   Higher scores indicate more complex text."
  [text]
  (let [sentences (str/split text #[\\.!] )]
       sentence-count (count (remove str/blank? sentences))
       words (tokenize text)
       word-count (count words)
       avg-sentence-length (if (zero? sentence-count) 0 (/ word-count sentence-count))
       avg-word-length (if (zero? word-count) 0 (/ (reduce + (map count words)) word-count))]
    (if (or (zero? sentence-count) (zero? word-count))
      0.0
      (+ (* 0.4 avg-sentence-length) (* 0.6 avg-word-length)))))

(defn text-complexity-metrics
  "Aggregate various complexity metrics for the given text."
  [text & {:keys [stop-words] :or {stop-words default-stop-words}}]
  {:readability-score (text-readability-score text)
   :vocabulary-size (vocabulary-size text :stop-words stop-words)
   :average-word-length (average-word-length text :stop-words stop-words)})