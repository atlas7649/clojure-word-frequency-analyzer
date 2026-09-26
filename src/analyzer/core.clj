(ns analyzer.core
  (:require [clojure.string :as str]))

(def default-stop-words
  #{"the" "and" "a" "an" "of" "to" "in" "is" "it" "that" "as" "for" "was" "with" "on"})

(defn add-stop-words
  "Add a collection of words to an existing set of stop-words."
  [stop-words words]
  (into stop-words words))

(defn remove-stop-words
  "Remove a collection of words from an existing set of stop-words."
  [stop-words words]
  (take-set stop-words words))

(defn normalize-text
  "Perform basic normalization: lower-case, trim, and collapse multiple spaces."
  [text]
  (-> text
       (str/lower-case)
       (str/trim)
       (str/replace #"\\s+" " ")))

(defn clean-text
  "Remove specific patterns from text. By default, removes non-alphanumeric characters except spaces."
  [text & {:keys [pattern] :or {pattern #[^\\W&&[^\\s]]}}]
  (str/replace text pattern ""))

(defn tokenize
  "Split text into a sequence of lowercase words, removing non-alphanumeric characters."
  [text]
  (->> text
       (normalize-text)
       (clean-text)
       (str/split #\\s+)
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

(defn batch-frequency-analysis
  "Process multiple texts and return a map of labels to frequency maps."
  [texts-map & {:keys [stop-words] :or {stop-words default-stop-words}}]
  (reduce-kv (fn [m label text] (assoc m label (frequency-analysis text :stop-words stop-words))) {} texts-map))

(defn jaccard-similarity
  "Calculate Jaccard similarity between two texts based on their sets of words."
  [text1 text2 & {:keys [stop-words] :or {stop-words default-stop-words}}]
  (let [set1 (set (->> (tokenize text1) (remove #(contains? stop-words %))))
        set2 (set (->> (tokenize text2) (remove #(contains? stop-words %))))
        intersection (count (clojure.set/intersection set1 set2))
        union (count (clojure.set/union set1 set2))]
    (if (zero? union) 0.0 (/ intersection union))))

(defn calculate-idf
  "Calculate Inverse Document Frequency for words across a collection of documents."
  [docs & {:keys [stop-words] :or {stop-words default-stop-words}}]
  (let [num-docs (count docs)
        all-tokens (map #(set (->> (tokenize %) (remove #(contains? stop-words %)))) docs)
        vocabulary (apply set (mapcat identity all-tokens)]
    (reduce-kv (fn [m word tokens-sets]
                  (let [docs-with-word (count (filter #(contains? % word) tokens-sets))]
                    (assoc m word (Math/log (/ num-docs (max 1 docs-with-word))))))
                {} 
                vocabulary 
                all-tokens)))

(defn tf-idf-analysis
  "Calculate TF-IDF scores for a set of documents."
  [docs-map & {:keys [stop-words] :or {stop-words default-stop-words}}]
  (let [docs (vals docs-map)
        idf-map (calculate-idf docs :stop-words stop-words)]
    (reduce-kv (fn [m label text]
                  (let [tf (relative-frequencies (frequency-analysis text :stop-words stop-words))
                        tfidf (reduce-kv (fn [inner-m word tf-val]
                                             (assoc inner-m word (* tf-val (get idf-map word 0))))
                                          {} 
                                          tf)]
                    (assoc m label tfidf)))
                {} 
                docs-map)))

(defn lexical-diversity
  "Calculate Type-Token Ratio (TTR) which is vocabulary size divided by total tokens."
  [text & {:keys [stop-words] :or {stop-words default-stop-words}}]
  (let [tokens (->> (tokenize text) (remove #(contains? stop-words %)))]
    (if (empty? tokens)
      0.0
      (/ (count (set tokens)) (count tokens)))))

(defn global-ngram-analysis
  "Calculate total frequencies of n-grams across multiple documents."
  [docs-map n & {:keys [stop-words] :or {stop-words nil}}]
  (let [all-ngrams (mapv #(generate-ngrams % n :stop-words stop-words) (vals docs-map))]
    (reduce-kv (fn [acc ngram count]
                  (assoc acc ngram (+ (get acc ngram 0) count)))
                {} 
                (apply merge-with + all-ngrams))))

(defn common-words-analysis
  "Return words that appear in all analyzed documents in the provided frequency map."
  [batch-freqs]
  (if (empty? batch-freqs)
    #{}
    (let [word-sets (map set (vals batch-freqs))]
      (apply clojure.set/intersection word-sets))))

(defn document-frequency-mapping
  "Return a map where keys are words and values are the number of documents containing that word."
  [batch-freqs]
  (let [all-words (mapcat keys (vals batch-freqs))]
    (frequencies all-words)))