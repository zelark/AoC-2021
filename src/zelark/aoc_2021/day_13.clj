(ns zelark.aoc-2021.day-13
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 13: Transparent Origami ---
;; https://adventofcode.com/2021/day/13

(def input (aoc/get-input 2021 13))

(defn parse-input [input]
  (let [[dots instructions] (str/split input #"\n\n")]
    [(->> (aoc/parse-longs dots)
          (partition 2)
          (set))
     (->> (re-seq #"(x|y)=(\d+)" instructions)
          (map (fn [[_ axis n]] [(keyword axis) (parse-long n)])))]))

(defn fold [dots [axis n]]
  (case axis
    :x (let [{right false left true} (group-by (fn [[x _y]] (< x n)) dots)]
         (into (set left)
               (map (fn [[x y]] [(- x (* (- x n) 2)) y]) right)))
    :y (let [{bottom false up true} (group-by (fn [[_x y]] (< y n)) dots)]
         (into (set up)
               (map (fn [[x y]] [x (- y (* (- y n) 2))]) bottom)))))

;; part 1
(let [[dots [finst]] (parse-input input)
      dots' (fold dots finst)]
  (count dots')) ; 759

;; part 2
(let [[dots instructions] (parse-input input)
      dots' (reduce #(fold %1 %2) dots instructions)
      max-x (apply max (map first dots'))
      max-y (apply max (map second dots'))]
  (-> (reduce #(aoc/mark-point %1 %2 \#)
              (aoc/empty-grid (inc max-x) (inc max-y))
              dots')
      (aoc/print-board))) ; HECRZKPR
