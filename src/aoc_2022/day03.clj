(ns aoc-2022.day03)
(require '[clojure.java.io :as io])
(require '[aoc-2022.utils :refer :all])

(defn compartments [sack]
  (let [n (count sack)]
    [(take (/ n 2) sack)
     (drop (/ n 2) sack)]))

(defn find-common [needles haystack]
  (let [needle (first needles)
        occurs (fn [hs] (filter #(= needle %) hs))]
    (cond
      (nil? needle) nil
      (seq(occurs haystack)) needle
      :else (find-common (rest needles) haystack))))

(defn priority-of [item]
  (cond
    (<= (int item) (int \Z)) (+ 27 (- (int item) (int \A)))
    :else (+ 1 (- (int item) (int \a)))))

(defn task1 [rows]
  (->> rows
       (map compartments)
       (map (fn [[c1 c2]] (find-common c1 c2)))
       (map priority-of)
       (reduce +)
       ))

(defn find-common2 [needles hs1 hs2]
  (let [needle (first needles)
        occurs (fn [hs] (filter #(= needle %) hs))]
    (cond
      (nil? needle)
          nil
      (and
       (seq (occurs hs1))
       (seq (occurs hs2)))
          needle
      :else
          (find-common2 (rest needles) hs1 hs2))))

(defn task2 [rows]
  (->> rows
       (partition 3)
       (map (fn [[c1 c2 c3]] (find-common2 c1 c2 c3)))
       (map priority-of)
       (reduce +)))

(with-open [rdr (io/reader "resources/day03_actual.txt")]
  (let [rows (line-seq rdr)]
    (println "task1:" (task1 rows))
    (println "task2:" (task2 rows))))
