(ns aoc-2022.day04)
(require '[clojure.java.io :as io])
(require '[aoc-2022.utils :as utils])
(require '[clojure.string :as str])

(defn parse-rows [rows]
  (->> rows
       (map #(str/split % #","))
       (map (fn [[a, b]]
              [(map #(Long/valueOf %) (str/split a #"-")),
               (map #(Long/valueOf %) (str/split b #"-"))]))
       (map flatten)))

(defn assignment-contains [[a1 a2 b1 b2]]
  (or
     (and (>= a1 b1) (<= a2 b2))
     (and (>= b1 a1) (<= b2 a2))))

(defn task1 [rows]
  (->> rows
       parse-rows
       (filter assignment-contains)
       count))

(defn assignment-overlaps [[a1 a2 b1 b2]]
  (not (or (> a1 b2) (> b1 a2))))

(defn task2 [rows]
  (->> rows
       parse-rows
       (filter assignment-overlaps)
       count))

(with-open [rdr (io/reader "resources/day04_actual.txt")]
  (let [rows (line-seq rdr)]
    (do
      (println "task1:" (task1 rows))
      (println "task2:" (task2 rows)))))
