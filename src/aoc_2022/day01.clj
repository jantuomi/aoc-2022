(ns aoc-2022.day01)
(require '[clojure.java.io :as io])

(defn split-by [delim acc coll]
  (let [head (first coll)
        tail (rest coll)]
    (if (nil? head)
      [acc]
      (cond
        (= delim head) (cons (reverse acc) (split-by delim [] tail))
        :else (split-by delim (cons head acc) tail)))))

(defn task1 [rows]
  (->> rows
       (map (fn [row] (if (not (empty? row)) (Long/valueOf row) "")))
       (split-by "" [])
       (map (fn [calories] (reduce + calories)))
       (reduce max)))

(defn task2 [rows]
  (->> rows
       (map (fn [row] (if (not (empty? row)) (Long/valueOf row) "")))
       (split-by "" [])
       (map (fn [calories] (reduce + calories)))
       sort
       reverse
       (take 3)
       (reduce +)))

(with-open [rdr (io/reader "resources/day01_actual.txt")]
  (let [rows (line-seq rdr)]
    (do
      (println "task1:" (task1 rows))
      (println "task2:" (task2 rows)))))
