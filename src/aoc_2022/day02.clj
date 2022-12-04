(ns aoc-2022.day02)
(require '[clojure.java.io :as io])
(require '[aoc-2022.utils :refer :all])

(defn score-round-result [own opp]
  (cond
    (= own opp) 3
    (and (= own :rock) (= opp :scissors)) 6
    (and (= own :rock) (= opp :paper)) 0
    (and (= own :paper) (= opp :scissors)) 0
    (and (= own :paper) (= opp :rock)) 6
    (and (= own :scissors) (= opp :paper)) 6
    (and (= own :scissors) (= opp :rock)) 0))

(def score-own-play
  {:rock     1
   :paper    2
   :scissors 3})

(def plays
  {\A :rock
   \B :paper
   \C :scissors
   \X :rock
   \Y :paper
   \Z :scissors})

(defn score-round [own opp]
  (+ (score-round-result own opp) (score-own-play own)))

(defn task1 [rows]
  (->> rows
       (map (fn [[opp _ own]] `(~(get plays own) ~(get plays opp))))
       (map (fn [[own opp]] (score-round own opp)))
       (reduce +)
       ))

(def results
  {\X :lose
   \Y :draw
   \Z :win})

(def own-play
  {'(:lose :rock)     :scissors
   '(:lose :paper)    :rock
   '(:lose :scissors) :paper
   '(:draw :rock)     :rock
   '(:draw :paper)    :paper
   '(:draw :scissors) :scissors
   '(:win  :rock)     :paper
   '(:win  :paper)    :scissors
   '(:win  :scissors) :rock})

(defn task2 [rows]
  (->> rows
       (map (fn [[opp _ res]] `(~(get results res) ~(get plays opp))))
       (map (fn [[res opp]] [(get own-play `(~res ~opp)) opp]))
       (map (fn [[own opp]] (score-round own opp)))
       (reduce +)))

(with-open [rdr (io/reader "resources/day02_actual.txt")]
  (let [rows (line-seq rdr)]
    (println "task1:" (task1 rows))
    (println "task2:" (task2 rows))))
