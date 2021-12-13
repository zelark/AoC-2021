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

(defn offset [c n]
  (- c (* (- c n) 2)))

(defn fold [dots [axis n]]
  (reduce (fn [acc [x y :as dot]]
            (conj acc
                  (case axis
                    :x (if (< x n) dot [(offset x n) y])
                    :y (if (< y n) dot [x (offset y n)]))))
          #{}
          dots))

;; part 1
(let [[dots [first-instuction]] (parse-input input)]
  (count (fold dots first-instuction))) ; 759

;; part 2
(let [[dots instructions] (parse-input input)]
  (-> (reduce fold dots instructions)
      (aoc/print-points))) ; HECRZKPR
