(require '[clojure.java.io :as io] 
            '[clojure.string :as str])

(defn tokenize [s]
  (->> (str/split (str/lower-case s) #"[^a-zäöüáéíóúãâêîôûàèìòùçñ]+")
       (remove stopwords)))

(defn term-frequencies [tokens]
  (let [freqs (frequencies tokens)
        term-count (count tokens)]
    (->> freqs
         (map (fn [[term frequency]]
                [term (/ frequency term-count]))
         (into {}))))

(defn idf [term corpus]
  (let [documents-matching-term (count (filter #(% term) corpus))]
    (if (> documents-matching-term 0)
      (-> (count corpus)
          (/ documents-matching-term)
          Math/log
          (+ 1))
      1.0)))

(defn tf-idf [document corpus]
  (->> (term-frequencies document)
       (map (fn [[term freq]]
              [term (* freq (idf term corpus))]))
       (into {})))

; Cosine Similarity
(defn dot-product [document another-document]
  (->> document
       (map (fn [[term tf-idf]]
              (* tf-idf (get another-document term 0.0))))
       (reduce +)))

(defn magnitude [document]
  (->> document
       (map (fn [[_ tf-idf]]
              (* tf-idf tf-idf)))
       (reduce +)
       Math/sqrt))

(defn cosine-similarity [document another-document]
  (let [dot-p (dot-product document another-document)
        document-magnitude (magnitude document)
        another-document-magnitude (magnitude (select-keys another-document (keys document)))
        magnitude-product (* document-magnitude another-document-magnitude)]
    (if (zero? magnitude-product)
      0.0
      (/ dot-p magnitude-product))))

(cosine-similarity
    {"doc0" 2.5 "This is a textual English document.. The quick brown fox jumps over the lazy dog. This is a document." 0.3}
    {"doc1" 2.5 "This is a textual English document. The quick white wolf eats the lazy sheep." 0.3}
    {"doc2" 2.5 "This is a textual English document. The slow brown fox jumps into the quizzical dog." 0.3}
    {"doc3" 2.5 "This is a textual English document. The slow white wolf lays next to the lazy dog." 0.3}
    {"doc4" 2.5 "This is a textual English document. The quick brown fox jumps over the lazy cat." 0.3}
)