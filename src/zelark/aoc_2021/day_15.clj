(ns zelark.aoc-2021.day-15
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.java.math :as math]
            [astar.core :as astar]))

;; --- Day 15: Chiton ---
;; https://adventofcode.com/2021/day/15

(def input (aoc/get-input 2021 15))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapcat (fn [y line] (keep-indexed (fn [x n] (-> [[x y] (Character/digit n 10)])) line)) (range))
       (into {})))

(defn adjacent [[x y]]
  (for [[dx dy] [[0 -1] [0 1] [-1 0] [1 0]]]
    [(+ x dx) (+ y dy)]))

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (math/abs (- x2 x1)) (math/abs (- y2 y1))))

(defn solve [input size]
  (let [risk-map (parse-input input)
        max-x (inc (apply max (map first (keys risk-map))))
        max-y (inc (apply max (map second (keys risk-map))))
        goal-x (dec (* size max-x))
        goal-y (dec (* size max-y))
        goal [goal-x goal-y]
        start [0 0]
        risk-map* (memoize
                   (fn [[x y]]
                     (nth (iterate #(inc (mod % 9))
                                   (risk-map [(mod x max-x) (mod y max-y)]))
                          (manhattan-distance start [(quot x max-x) (quot y max-y)]))))
        graph (fn [n]
                (filter (fn [[x y]]
                          (and (<= 0 x goal-x)
                               (<= 0 y goal-y)))
                        (adjacent n)))
        dist (fn [_ to] (risk-map* to))
        h (partial manhattan-distance goal)]
    (->> (astar/route graph dist h start goal)
         (map risk-map*)
         (reduce +))))

;; part 1
(solve input 1) ; 811

;; part 2
(solve input 5) ; 3012
