(ns zelark.aoc-2021.day-06
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 6: Lanternfish ---
;; https://adventofcode.com/2021/day/6

(def input (aoc/get-input 2021 06))

(defn parse-input [input]
  (->> (re-seq #"\d" input)
       (mapv parse-long)))

(def count-fish
  (memoize
   (fn [days fish-timer]
     (->> (iterate #(- % 7) (- days (inc fish-timer)))
          (take-while (complement neg?))
          (map #(count-fish % 8))
          (reduce + 1)))))

;; part 1
(->> (parse-input input)
     (map (partial count-fish 80))
     (reduce +)) ; 393019
       
;; part 2
(->> (parse-input input)
     (map (partial count-fish 256))
     (reduce +)) ; 1757714216975

;; A solution I borrow from @mchampine and adapted a bit.
(defn step [state]
  (-> (mapv state [1 2 3 4 5 6 7 8 0])
      (update 6 (fnil + 0 0) (get state 0))))

(defn calc-fish [input days]
  (->> (frequencies input)
       (iterate step)
       (drop days)
       (first)
       (reduce +)))

(calc-fish (parse-input input) 80)
(calc-fish (parse-input input) 256)
