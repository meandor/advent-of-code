(ns day2.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

(defn parse-line [line]
  (let [[_ min-occurrence max-occurrence letter password]
        (re-matches #"(\d+)-(\d+) (\w+): (\w+)" line)]
    {:password     password
     :first-index  (read-string min-occurrence)
     :second-index (read-string max-occurrence)
     :letter       letter}))

(defn count-letter-occurrence [letter word]
  (->> word
       (map #(str %))
       (filter #(= letter %))
       (count)))

(defn valid-sled-password? [{:keys [password first-index second-index letter]}]
  (<=
    first-index
    (count-letter-occurrence letter password)
    second-index))

(defn- letter-at-index [index password-sequence]
  (nth password-sequence (dec index)))

(defn- xor [A B]
  (or (and A (not B))
      (and B (not A))))

(defn valid-toboggan-password? [{:keys [password first-index second-index letter]}]
  (let [password-sequence (map #(str %) password)]
    (xor (= letter (letter-at-index first-index password-sequence))
         (= letter (letter-at-index second-index password-sequence)))))

(defn count-valid-passwords [password-policy-fn passwords-with-rules]
  (count (filter password-policy-fn passwords-with-rules)))

(defn print-valid-passwords [print-prefix password-policy-fn passwords-with-rules]
  (->> passwords-with-rules
       (count-valid-passwords password-policy-fn)
       (str "number of valid " print-prefix " passwords: ")
       (println))
  passwords-with-rules)

(defn -main [& args]
  (if (= 1 (count args))
    (with-open [rdr (io/reader (first args))]
      (->> (line-seq rdr)
           (map parse-line)
           (print-valid-passwords "sled" valid-sled-password?)
           (print-valid-passwords "toboggan" valid-toboggan-password?)))
    (println "usage: java -jar day2.jar [input.txt-location]")))
