(ns day2.core
  (:gen-class)
  (:require [clojure.java.io :as io]))

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

(defn valid-sled-password? [{:keys [password min-occurrence max-occurrence letter]}]
  (<=
    min-occurrence
    (count-letter-occurrence letter password)
    max-occurrence))

(defn count-valid-passwords [passwords-with-rules password-policy-fn]
  (count (filter password-policy-fn passwords-with-rules)))

(defn -main [& args]
  (if (= 1 (count args))
    (with-open [rdr (io/reader (first args))]
      (->> (line-seq rdr)
           (map parse-line)
           (count-valid-passwords valid-sled-password?)
           (str "number of valid sled passwords: ")
           (println)))
    (println "usage: java -jar day2.jar [input.txt location]")))
