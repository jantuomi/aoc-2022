(ns aoc-2022.day05)
(require '[aoc-2022.utils :refer :all])

(defn split-by-empty-line [rows]
  (split-by "" rows))

(defn revv [v] (vec (rseq v)))

(defn parse-stacks [stacks]
  (->> stacks
       transpose
       (keep-indexed #(if (zero? (mod (- %1 1) 4)) %2 nil))
       (map #(filter (partial not= \ ) %))
       (map #(drop-last %))
       (map vec)))

(defn parse-instrs [instrs]
  (->> instrs
       (map #(split-by \  %))
       (map (partial keep-indexed #(if (odd? %1) (apply str %2) nil)))
       (map (partial map to-long))
       (map vec)
       ))

(defn prepare [rows]
  (let [[stack-rows instr-rows] (split-by-empty-line rows)
        stacks (parse-stacks stack-rows)
        instrs (parse-instrs instr-rows)]
    (list (vec stacks) (vec instrs))))

(defn run-instr1 [[n from to] stacks]
  (if (zero? n)
    stacks
    (let [from-stack (stacks (dec from))
          to-stack (stacks (dec to))
          val (first from-stack)
          new-from-stack (vec (rest from-stack))
          new-to-stack (vec (cons val to-stack))
          new-stacks' (assoc stacks (dec from) new-from-stack)
          new-stacks (assoc new-stacks' (dec to) new-to-stack)]
      (run-instr1 [(dec n) from to] new-stacks))))

(defn run-instrs1 [[instr & is] stacks]
  (if (seq instr)
    (run-instrs1 (vec is) (run-instr1 instr stacks))
    stacks))

(defn task1 [rows]
  (let [[stacks instrs] (prepare rows)]
    (->> (run-instrs1 instrs stacks)
         (map first)
         (apply str))))

(defn run-instr2 [[n from to] stacks]
  (let [from-stack (stacks (dec from))
        to-stack (stacks (dec to))
        vals (vec (take n from-stack))
        new-from-stack (vec (drop n from-stack))
        new-to-stack (vec (concat vals to-stack))
        new-stacks' (assoc stacks (dec from) new-from-stack)]
    (assoc new-stacks' (dec to) new-to-stack)))

(defn run-instrs2 [[instr & is] stacks]
  (if (seq instr)
    (run-instrs2 (vec is) (run-instr2 instr stacks))
    stacks))

(defn task2 [rows]
  (let [[stacks instrs] (prepare rows)]
    (->> (run-instrs2 instrs stacks)
         (map first)
         (apply str))))

(run-aoc "day05" task1 task2)