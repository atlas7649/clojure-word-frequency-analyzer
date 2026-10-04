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

(defn simple-stem
  "A very basic suffix stripper to group similar words (e.g., 'running' -> 'run')."
  [word]
  (cond
    (str/ends-with? word "ing") (str/substring word 0 (- (count word) 3))
    (str/ends-with? word "ed") (str/substring word 0 (- (count word) 2))
    (str/ends-with? word "ies") (str/replace word #"ies$" "y")
    (str/ends-with? word "s") (if (not= (count word) 1) (str/substring word 0 (dec (count word))) word)
    :else word))

(defn tokenize
  "Split text into a sequence of lowercase words, removing non-alphanumeric characters."
  [text & {:keys [stem] :or {stem false}}]
  (let [tokens (->> text
                    (normalize-text)
                    (clean-text)
                    (str/split #\\s+)
                    (remove empty?))]
    (if stem
      (map simple-stem tokens)
      tokens)))

(defn frequency-analysis
  "Calculate word frequencies, optionally filtering out stop words."
  [text & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (->> (tokenize text :stem stem)
       (remove #(contains? stop-words %))
       (frequencies)))

(defn vocabulary-size
  "Calculate the number of unique words in the text, optionally filtering stop words."
  [text & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (count (set (->> (tokenize text :stem stem)
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
  [text n & {:keys [stop-words stem] :or {stop-words nil stem false}}]
  (let [words (tokenize text :stem stem)
        filtered-words (if stop-words (remove #(contains? stop-words %) words) words)]
    (->> filtered-words
         (partition n 1)
         (frequencies))))

(defn dominant-ngram
  "Find the most frequent n-gram of size n in the text."
  [text n & {:keys [stop-words stem] :or {stop-words nil stem false}}]
  (let [ngrams (generate-ngrams text n :stop-words stop-words :stem stem)]
    (first (sorted-frequencies ngrams))))

(defn word-length-distribution
  "Calculate the frequency of word lengths in the text, optionally filtering stop words."
  [text & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (->> (tokenize text :stem stem)
       (remove #(contains? stop-words %))
       (map count)
       (frequencies)))

(defn average-word-length
  "Calculate the average length of words in the text, optionally filtering stop words."
  [text & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [words (->> (tokenize text :stem stem)
                    (remove #(contains? stop-words %)))]
    (if (empty? words)
      0
      (/ (reduce + (map count words)) (count words)))))

(defn text-summary
  "Return a summary containing vocabulary size and the top N most frequent words."
  [text n & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [freqs (frequency-analysis text :stop-words stop-words :stem stem)]
    {:vocabulary-size (vocabulary-size text :stop-words stop-words :stem stem)
     :top-words (most-common freqs n)}))

(defn keyword-density
  "Calculate the density of specific keywords in the text relative to the total word count."
  [text keywords & {:keys [stem] :or {stem false}}]
  (let [words (tokenize text :stem stem)
        total (count words)]
    (if (zero? total)
      (reduce (fn [m k] (assoc m k 0.0)) {} keywords)
      (let [freqs (frequencies words)]
        (reduce (fn [m k]
                  (let [key-val (if stem (simple-stem (str/lower-case k)) (str/lower-case k))
                        count (get freqs key-val 0)]
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

(defn gunning-fog-index
  "Calculate the Gunning Fog Index for a text.
   Formula: 0.4 * ((average sentence length) + (percentage of complex words))
   Complex words are defined as words with 3 or more syllables (approximated here by length > 6)."
  [text]
  (let [sentences (str/split text #[\\.!] )]
       sentence-count (count (remove str/blank? sentences))
       words (tokenize text)
       word-count (count words)
       complex-words (count (filter #(> (count %) 6) words))
       avg-sentence-length (if (zero? sentence-count) 0 (/ word-count sentence-count))
       pct-complex (if (zero? word-count) 0 (* 100 (/ complex-words word-count)))]
    (if (or (zero? sentence-count) (zero? word-count))
      0.0
      (* 0.4 (+ avg-sentence-length pct-complex)))))

(defn text-complexity-metrics
  "Aggregate various complexity metrics for the given text."
  [text & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  {:readability-score (text-readability-score text)
   :gunning-fog-index (gunning-fog-index text)
   :vocabulary-size (vocabulary-size text :stop-words stop-words :stem stem)
   :average-word-length (average-word-length text :stop-words stop-words :stem stem)
   :entropy (shannon-entropy text :stop-words stop-words :stem stem)})

(defn batch-frequency-analysis
  "Process multiple texts and return a map of labels to frequency maps."
  [texts-map & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (reduce-kv (fn [m label text] (assoc m label (frequency-analysis text :stop-words stop-words :stem stem))) {} texts-map))

(defn jaccard-similarity
  "Calculate Jaccard similarity between two texts based on their sets of words."
  [text1 text2 & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [set1 (set (->> (tokenize text1 :stem stem) (remove #(contains? stop-words %))))
        set2 (set (->> (tokenize text2 :stem stem) (remove #(contains? stop-words %))))
        intersection (count (clojure.set/intersection set1 set2))
        union (count (clojure.set/union set1 set2))]
    (if (zero? union) 0.0 (/ intersection union))))

(defn cosine-similarity
  "Calculate Cosine Similarity between two texts based on word frequency vectors."
  [text1 text2 & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [f1 (frequency-analysis text1 :stop-words stop-words :stem stem)
        f2 (frequency-analysis text2 :stop-words stop-words :stem stem)
        all-words (set (concat (keys f1) (keys f2)))
        dot-product (reduce + (map (fn [w] (* (get f1 w 0) (get f2 w 0))) all-words))
        mag1 (Math/sqrt (reduce + (map (fn [v] (* v v)) (vals f1))))
        mag2 (Math/sqrt (reduce + (map (fn [v] (* v v)) (vals f2))))]
    (if (or (zero? mag1) (zero? mag2))
      0.0
      (/ dot-product (* mag1 mag2)))))

(defn manhattan-distance
  "Calculate the Manhattan distance (L1 norm) between word frequency vectors of two texts."
  [text1 text2 & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [f1 (frequency-analysis text1 :stop-words stop-words :stem stem)
        f2 (frequency-analysis text2 :stop-words stop-words :stem stem)
        all-words (set (concat (keys f1) (keys f2)))]
    (reduce + (map (fn [w] (Math/abs (- (get f1 w 0) (get f2 w 0)))) all-words))))

(defn euclidean-distance
  "Calculate the Euclidean distance (L2 norm) between word frequency vectors of two texts."
  [text1 text2 & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [f1 (frequency-analysis text1 :stop-words stop-words :stem stem)
        f2 (frequency-analysis text2 :stop-words stop-words :stem stem)
        all-words (set (concat (keys f1) (keys f2)))]
    (Math/sqrt (reduce + (map (fn [w] (let [diff (- (get f1 w 0) (get f2 w 0))] (* diff diff))) all-words))))

(defn hamming-distance
  "Calculate the Hamming distance between two texts based on the set of words present (binary vector)."
  [text1 text2 & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [set1 (set (->> (tokenize text1 :stem stem) (remove #(contains? stop-words %))))
        set2 (set (->> (tokenize text2 :stem stem) (remove #(contains? stop-words %))))
        all-words (clojure.set/union set1 set2)]
    (count (filter (fn [w] (not= (contains? set1 w) (contains? set2 w))) all-words))))

(defn canberra-distance
  "Calculate Canberra distance between word frequency vectors of two texts.
   d = sum(|pi - qi| / (|pi| + |qi|))
   Handles zero denominators by treating the term as 0."
  [text1 text2 & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [f1 (frequency-analysis text1 :stop-words stop-words :stem stem)
        f2 (frequency-analysis text2 :stop-words stop-words :stem stem)
        all-words (set (concat (keys f1) (keys f2)))]
    (reduce + (map (fn [w]
                      (let [v1 (get f1 w 0)
                            v2 (get f2 w 0)
                            denom (+ (Math/abs v1) (Math/abs v2))]
                        (if (zero? denom) 0.0 (/ (Math/abs (- v1 v2)) denom))))
                    all-words))))

(defn bray-curtis-dissimilarity
  "Calculate Bray-Curtis dissimilarity between word frequency vectors of two texts.
   d = sum(|pi - qi|) / sum(|pi + qi|)"
  [text1 text2 & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [f1 (frequency-analysis text1 :stop-words stop-words :stem stem)
        f2 (frequency-analysis text2 :stop-words stop-words :stem stem)
        all-words (set (concat (keys f1) (keys f2)))
        sum-diff (reduce + (map (fn [w] (Math/abs (- (get f1 w 0) (get f2 w 0)))) all-words))
        sum-total (reduce + (map (fn [w] (+ (get f1 w 0) (get f2 w 0))) all-words))]
    (if (zero? sum-total) 0.0 (/ sum-diff sum-total))))

(defn text-similarity-report
  "Return a map containing multiple similarity and distance metrics between two texts."
  [text1 text2 & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  {:jaccard (jaccard-similarity text1 text2 :stop-words stop-words :stem stem)
   :cosine (cosine-similarity text1 text2 :stop-words stop-words :stem stem)
   :manhattan (manhattan-distance text1 text2 :stop-words stop-words :stem stem)
   :euclidean (euclidean-distance text1 text2 :stop-words stop-words :stem stem)
   :hamming (hamming-distance text1 text2 :stop-words stop-words :stem stem)
   :canberra (canberra-distance text1 text2 :stop-words stop-words :stem stem)
   :bray-curtis (bray-curtis-dissimilarity text1 text2 :stop-words stop-words :stem stem)}))

(defn kullback-leibler-divergence
  "Calculate the KL Divergence between the word distributions of two texts.
   D_KL(P || Q) = sum(P(i) * log(P(i) / Q(i)))
   A small epsilon is added to Q to avoid division by zero."
  [text1 text2 & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [p (relative-frequencies (frequency-analysis text1 :stop-words stop-words :stem stem))
        q (relative-frequencies (frequency-analysis text2 :stop-words stop-words :stem stem))
        epsilon 1e-10
        all-words (keys p)]
    (if (empty? p)
      0.0
      (reduce + (map (fn [w]
                       (let [p-val (get p w)
                             q-val (max epsilon (get q w 0))]
                         (* p-val (Math/log (/ p-val q-val)))))
                     all-words))))))

(defn calculate-idf
  "Calculate Inverse Document Frequency for words across a collection of documents."
  [docs & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [num-docs (count docs)
        all-tokens (map #(set (->> (tokenize % :stem stem) (remove #(contains? stop-words %)))) docs)
        vocabulary (apply set (mapcat identity all-tokens))]
    (reduce-kv (fn [m word _]
                  (let [docs-with-word (count (filter #(contains? % word) all-tokens))]
                    (assoc m word (Math/log (/ num-docs (max 1 docs-with-word))))))
                {} 
                vocabulary 
                nil)))

(defn tf-idf-analysis
  "Calculate TF-IDF scores for a set of documents."
  [docs-map & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [docs (vals docs-map)
        idf-map (calculate-idf docs :stop-words stop-words :stem stem)]
    (reduce-kv (fn [m label text]
                  (let [tf (relative-frequencies (frequency-analysis text :stop-words stop-words :stem stem))
                        tfidf (reduce-kv (fn [inner-m word tf-val]
                                             (assoc inner-m word (* tf-val (get idf-map word 0))))
                                          {} 
                                          tf)]
                    (assoc m label tfidf)))
                {} 
                docs-map)))

(defn tf-idf-top-terms
  "Extract the top N terms for each document based on TF-IDF scores."
  [docs-map n & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [tfidf-results (tf-idf-analysis docs-map :stop-words stop-words :stem stem)]
    (reduce-kv (fn [m label scores]
                  (assoc m label (most-common scores n)))
                {} 
                tfidf-results)))

(defn lexical-diversity
  "Calculate Type-Token Ratio (TTR) which is vocabulary size divided by total tokens."
  [text & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [tokens (->> (tokenize text :stem stem) (remove #(contains? stop-words %)))]
    (if (empty? tokens)
      0.0
      (/ (count (set tokens)) (count tokens)))))

(defn herdan-vocabulary
  "Calculate Herdan's Vocabulary (TTR over a sequence of token windows).
   Returns a sequence of TTR values for windows of size window-size."
  [text window-size & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [tokens (->> (tokenize text :stem stem) (remove #(contains? stop-words %)))]
    (->> tokens
         (partition window-size 1)
         (map (fn [window]
                 (if (empty? window)
                   0.0
                   (/ (count (set window)) (count window))))))))

(defn shannon-entropy
  "Calculate the Shannon entropy of the word distribution in the text."
  [text & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [freqs (frequency-analysis text :stop-words stop-words :stem stem)
        probs (relative-frequencies freqs)]
    (if (empty? probs)
      0.0
      (reduce + (map (fn [[_ p]] (* p (Math/log p))) (seq probs))))))

(defn global-ngram-analysis
  "Calculate total frequencies of n-grams across multiple documents."
  [docs-map n & {:keys [stop-words stem] :or {stop-words nil stem false}}]
  (let [all-ngrams (mapv #(generate-ngrams % n :stop-words stop-words :stem stem) (vals docs-map))]
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

(defn zipfs-law-analysis
  "Analyze if the word distribution follows Zipf's Law.
   Returns a sequence of [rank frequency predicted-frequency] for the top N words."
  [text n & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [freqs (frequency-analysis text :stop-words stop-words :stem stem)
        sorted (sorted-frequencies freqs)
        total-tokens (reduce + (vals freqs))
        most-freq-count (if (empty? sorted) 0 (second (first sorted)))]
    (if (or (empty? sorted) (zero? total-tokens))
      []
      (map-indexed (fn [idx [word count]]
                      (let [rank (inc idx)
                            predicted (/ most-freq-count rank)]
                        [rank count predicted]))
                    (take n sorted)))))

(defn word-cloud-data
  "Generate data structured for word cloud visualization: a sequence of {text, value} maps."
  [text n & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [freqs (frequency-analysis text :stop-words stop-words :stem stem)]
    (->> (most-common freqs n)
         (map (fn [[word count]] {:text word :value count})))))

(defn text-to-freq-map
  "Helper to convert a text block directly to a frequency map with specified stop words."
  [text stop-words & {:keys [stem] :or {stem false}}]
  (frequency-analysis text :stop-words stop-words :stem stem))

(defn cluster-documents
  "Group documents into clusters based on a minimum cosine similarity threshold.
   Returns a vector of clusters, where each cluster is a vector of document labels."
  [docs-map threshold & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [labels (vec (keys docs-map))]
    (loop [remaining labels
           clusters []]
      (if (empty? remaining)
        clusters
        (let [current (first remaining)
              others (rest remaining)
              cluster (cons current
                           (reduce (fn [acc other]
                                       (if (>= (cosine-similarity (get docs-map current) (get docs-map other) :stop-words stop-words :stem stem) threshold)
                                         (conj acc other)
                                         acc))
                                     []
                                     others))
              new-remaining (remove #(contains? (set cluster) %) others)]
          (recur new-remaining (conj clusters cluster))))))))

(defn text-to-vector
  "Convert text to a frequency vector based on a provided vocabulary."
  [text vocabulary & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [freqs (frequency-analysis text :stop-words stop-words :stem stem)]
    (map #(get freqs % 0) vocabulary)))

(defn chebyshev-distance
  "Calculate the Chebyshev distance (L-infinity norm) between two vectors."
  [v1 v2]
  (apply max (map #(Math/abs (- % %)) (map vector v1 v2))))

(defn minkowski-distance
  "Calculate the Minkowski distance between two vectors for a given p."
  [v1 v2 p]
  (Math/pow (reduce + (map (fn [[x y]] (Math/pow (Math/abs (- x y)) p)) (map vector v1 v2))) (/ 1.0 p)))

(defn vector-distance-report
  "Return a map of various distance metrics between two vectors."
  [v1 v2 & {:keys [p] :or {p 3}}]
  {:manhattan (reduce + (map (fn [[x y]] (Math/abs (- x y))) (map vector v1 v2)))
   :euclidean (Math/sqrt (reduce + (map (fn [[x y]] (let [d (- x y)] (* d d))) (map vector v1 v2))))
   :chebyshev (chebyshev-distance v1 v2)
   :minkowski (minkowski-distance v1 v2 p)})

(defn most-significant-words
  "Identify the most significant words based on frequency * length. 
   This helps highlight content-bearing words over common short words."
  [text n & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [freqs (frequency-analysis text :stop-words stop-words :stem stem)]
    (->> freqs
         (map (fn [[word count]] [word (* count (count word))]))
         (sort-by second)
         (reverse)
         (take n))))

(defn extractive-summarize
  "Generate an extractive summary of the text by ranking sentences based on the TF-IDF of their words.
   Takes the top `n` sentences."
  [text n & {:keys [stop-words stem] :or {stop-words default-stop-words stem false}}]
  (let [sentences (str/split text #[\\.!] ) 
        clean-sentences (remove str/blank? sentences)
        docs-map (into {} (map-indexed (fn [i s] [i s]) clean-sentences))
        tfidf-scores (tf-idf-analysis docs-map :stop-words stop-words :stem stem)]
    (->> clean-sentences
         (map-indexed (fn [idx sentence]
                         (let [words (tokenize sentence :stem stem)
                               score (reduce + (map (fn [w] (get-in tfidf-scores [idx w] 0.0)) words))]
                           [idx score sentence])))
         (sort-by second)
         (reverse)
         (take n)
         (sort-by first)
         (map third)
         (str/join ". ")
         (str/trim)
         (fn [s] (if (empty? s) "" (str s "."))))))