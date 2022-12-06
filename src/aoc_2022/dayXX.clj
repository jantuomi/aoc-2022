(ns aoc-2022.dayXX)

(require '[aoc-2022.utils :refer :all])

(defn task1 [rows]
  (->> rows
       count))

(defn task2 [rows]
  (->> rows
       count))

(run-aoc "dayXX" task1 task2)
