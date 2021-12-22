(ns zelark.aoc-2021.day-05
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 5: Hydrothermal Venture ---
;; https://adventofcode.com/2021/day/5

(def input (aoc/get-input 2021 05))

(defn parse-line [s]
  (->> (re-seq #"\d+" s)
       (mapv parse-long)))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-line)))

(defn straight-line? [[x1 y1 x2 y2]]
  (or (== x1 x2) (== y1 y2)))

(defn straight-line-points [[x1 y1 x2 y2]]
  (for [x (aoc/rangex x1 x2)
        y (aoc/rangex y1 y2)]
    [x y]))

(defn diagonal-line-points [[x1 y1 x2 y2]]
  (map vector (aoc/rangex x1 x2) (aoc/rangex y1 y2)))

(defn line-points [line]
  (if (straight-line? line)
    (straight-line-points line)
    (diagonal-line-points line)))

(defn solve [lines]
  (->> (mapcat line-points lines)
       (frequencies)
       (vals)
       (remove #(== % 1))
       (count)))

;; part 1
(->> (parse-input input)
     (filter straight-line?)
     (solve)) ; 5632
       
;; part 2
(->> (parse-input input)
     (solve)) ; 22213
