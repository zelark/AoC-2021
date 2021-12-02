(ns zelark.aoc-2021.day-02
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 2: Dive! ---
;; https://adventofcode.com/2021/day/2

(def input (aoc/get-input 2021 02))

(defn parse-input [input]
  (->> (re-seq #"(\w+) (\d+)" input)
       (map (fn [[_ cmd x]]
              [(keyword cmd) (parse-long x)]))))

;; part 1
(defn step-1 [[hpos depth] [cmd x]]
  (case cmd
    :forward [(+ hpos x) depth]
    :up      [hpos (- depth x)]
    :down    [hpos (+ depth x)]))

(->> (parse-input input)
     (reduce step-1 [0 0])
     (apply *)) ; 1727835

;; part 2
(defn step-2 [[hpos depth aim] [cmd x]]
  (case cmd
    :forward [(+ hpos x) (+ depth (* aim x)) aim]
    :up      [hpos depth (- aim x)]
    :down    [hpos depth (+ aim x)]))

(->> (parse-input input)
     (reduce step-2 [0 0 0])
     (take 2)
     (apply *))  ; 1544000595
