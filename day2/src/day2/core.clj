(ns day2.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

(defn parse-line [line]
  (let [[_ min-occurrence max-occurrence letter password]
        (re-matches #"(\d+)-(\d+) (\w+): (\w+)" line)]
    {:password       password
     :min-occurrence (read-string min-occurrence)
     :max-occurrence (read-string max-occurrence)
     :letter         letter}))

(defn count-letter-occurrence [letter word]
  (->> (seq word)
       (map #(str %))
       (filter #(= letter %))
       (count)))

(defn valid-password? [{:keys [password min-occurrence max-occurrence letter]}]
  (<=
    min-occurrence
    (count-letter-occurrence letter password)
    max-occurrence))
