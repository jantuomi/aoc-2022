(ns aoc-2022.day06)
(require '[aoc-2022.utils :refer :all])

(defn process
  ([n cs] (process n 0 (list) cs))
  ([n idx acc [c & cs]]
   (if (= n (count (distinct acc)))
     idx
     (process n (inc idx) (take n (cons c acc)) cs))))

(defn task1 [rows]
  (->> rows
       first
       (process 4)))

(defn task2 [rows]
  (->> rows
       first
       (process 14)))

(run-aoc "day06" task1 task2)
