(ns zelark.aoc-2021.day-01
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 1: Sonar Sweep ---
;; https://adventofcode.com/2021/day/1

(def input (aoc/get-input 2021 01))

(defn parse-input [input]
  (->> (re-seq #"\d+" input)
       (map parse-long)))

(defn solve [input]
  (->> (partition 2 1 input)
       (map (fn [[a b]] (< a b)))
       (filter true?)
       (count)))

;; part 1
(-> (parse-input input)
    (solve)) ; 1184

;; part 2
(->> (parse-input input)
     (partition 3 1)
     (map #(apply + %))
     (solve)) ; 1158
