(ns zelark.aoc-2021.day-10
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 10: Syntax Scoring ---
;; https://adventofcode.com/2021/day/10

(def input (aoc/get-input 2021 10))

(defn balance [form]
  (let [brackets {\[ \] \( \) \{ \} \< \>}
        scan (fn [q x]
               (if (brackets x)
                 (conj q x)
                 (if (= (brackets (peek q)) x)
                   (pop q)
                   (reduced {:corrupted x}))))
        ret (reduce scan [] form)]
    (if (vector? ret)
      {:incomplete (mapv brackets (rseq ret))}
      ret)))

;; part 1
(->> (str/split-lines input)
     (keep (comp :corrupted balance))
     (map {\) 3 \] 57 \} 1197 \> 25137})
     (reduce + 0)) ; 193275

;; part 2
(let [calc-score (fn [xs]
                   (->> (map {\) 1 \] 2 \} 3 \> 4} xs)
                        (reduce (fn [acc x] (+ (* acc 5) x)) 0)))
      scores (->> (str/split-lines input)
                  (keep (comp :incomplete balance))
                  (map calc-score))
      middle (quot (count scores) 2)]
  (->> (sort scores)
       (drop middle)
       (first))) ; 2429644557
