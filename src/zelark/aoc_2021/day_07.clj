(ns zelark.aoc-2021.day-07
  (:require [zelark.aoc.core :as aoc]
            [clojure.java.math :as math]))

;; --- Day 7: The Treachery of Whales ---
;; https://adventofcode.com/2021/day/7

(def input (aoc/get-input 2021 07))

(defn parse-input [input]
  (->> (re-seq #"\d+" input)
       (mapv parse-long)))

(defn dist [pos1 pos2]
  (math/abs (- pos1 pos2)))

(defn calc-total-fuel [fuel-fn apos pos->n]
  (reduce-kv (fn [acc pos n]
               (+ acc (* (fuel-fn (dist apos pos)) n)))
             0
             pos->n))

(defn solve [fuel-fn input]
  (let [positions (parse-input input)
        [min-pos max-pos] (apply (juxt min max) positions)
        freqs (frequencies positions)]
    (->> (range min-pos (inc max-pos))
         (map #(calc-total-fuel fuel-fn % freqs))
         (apply min))))

;; part 1
(solve identity input) ; 364898

;; part 2
(defn fuel [n]
  (/ (* n (inc n)) 2)) ; thanks @mikelis for this math ^_^.

(solve fuel input) ; 104149091
