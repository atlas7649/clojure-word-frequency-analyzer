(ns analyzer.io
  (:require [clojure.java.io :as io]
            [analyzer.core :as analyzer]))

(defn read-file-text
  "Reads the content of a file into a string."
  [file-path]
  (with-open [r (io/reader file-path)]
    (.readString r)))

(defn analyze-text
  "Performs frequency analysis on a provided string."
  [text & {:keys [stop-words stem] :or {stop-words nil stem false}}]
  (analyzer/frequency-analysis text :stop-words stop-words :stem stem))

(defn analyze-file
  "Performs frequency analysis on a file."
  [file-path & {:keys [stop-words stem] :or {stop-words nil stem false}}]
  (let [text (read-file-text file-path)]
    (analyze-text text :stop-words stop-words :stem stem)))

(defn analyze-files
  "Performs frequency analysis on multiple files, returning a map of filename to results."
  [file-paths & {:keys [stop-words stem] :or {stop-words nil stem false}}]
  (into {} 
        (for [path file-paths]
          [(io/file-name path) (analyze-file path :stop-words stop-words :stem stem)])))

(defn analyze-directory
  "Recursively analyzes all .txt files in a directory, returning an aggregated frequency map."
  [dir-path & {:keys [stop-words stem] :or {stop-words nil stem false}}]
  (let [dir (io/file dir-path)
        files (filter #(and (.isFile %) (.endsWith (.getName %) ".txt")) 
                      (file-seq dir))]
    (reduce (fn [acc file]
              (let [freqs (analyze-file (.getAbsolutePath file) :stop-words stop-words :stem stem)]
                (reduce-kv (fn [inner-acc word count]
                              (assoc inner-acc word (+ count (get inner-acc word 0))))
                            acc freqs)))
            {} files)))