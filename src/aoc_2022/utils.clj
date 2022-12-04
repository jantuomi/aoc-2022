(ns aoc-2022.utils)
(require '[clojure.string :as str])

; Debugging

(def spy #(do (println "DEBUG:" %) %))

; Number parsing

(def to-long #(Long/valueOf %))
(def to-float #(Float/valueOf %))

; Collections

(defn- split-by' [delim acc [x & xs]]
  (condp = x
    nil                       (list (reverse acc))
    delim (cons (reverse acc) (split-by' delim '() xs))
    ; otherwise
    (split-by' delim (cons x acc) xs)))

(def split-by #(split-by' %1 '() %2))

; Advent of Code specific

(defn read-aoc [day]
  (let [slurp-f #(str/split-lines (slurp (format % day)))]
    {:sample (slurp-f "resources/%s_sample.txt")
     :actual (slurp-f "resources/%s_actual.txt")}))

(defn run-aoc
  "Reads input files for `day` and invokes given `task1` and `task2` functions with the contents.
   `task1` and `task2` should take a list of strings as their first argument."
  [day task1 task2]
  (let [{:keys [sample actual]} (read-aoc day)]
    (println "task1 @ sample:" (task1 sample))
    (println "task1 @ actual:" (task1 actual))
    (println "task2 @ sample:" (task2 sample))
    (println "task2 @ actual:" (task2 actual))))

; Functions

(defn chain
  "forward compose: comp fns in reverse order"
  [& fns]
  (apply comp (reverse fns)))

; Logic

(defmacro nand
  "Logical NAND. `(nand a b c)` is equivalent to `(not (and a b c))`."
  [& args]
  (list 'not (cons 'and args)))

(defmacro nor
  "Logical NOR. `(nor a b c)` is equivalent to `(not (or a b c))`."
  [& args]
  (list 'not (cons 'or args)))
