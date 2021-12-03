(ns zelark.aoc-2021.day-03
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 3: Binary Diagnostic ---
;; https://adventofcode.com/2021/day/3

(def input (aoc/get-input 2021 03))

(defn parse-bin [s]
  (Long/parseLong s 2))

;; part 1
(->> (str/split-lines input)
     (apply map #(->> (frequencies %&)
                      (sort-by val)
                      (map first)))
     (apply map vector)
     (map (comp parse-bin str/join))
     (apply *)) ; 1307354

;; part 2
(defn make-bit-fn [winner]
  (fn common-bit [numbers n]
    (->> (map #(nth % n) numbers)
         (frequencies)
         (sort-by (juxt second first))
         (winner))))

(defn find-rating [numbers bit-fn]
  (-> (reduce (fn [acc n]
                (if (== (count acc) 1)
                  (reduced acc)
                  (let [bit (bit-fn acc n)]
                    (filter #(= (nth % n) bit) acc))))
              numbers
              (range (count (first numbers))))
      (first)))

(let [numbers    (str/split-lines input)
      o2-rating  (find-rating numbers (make-bit-fn (comp first second)))
      co2-rating (find-rating numbers (make-bit-fn ffirst))]
  (* (parse-bin o2-rating)
     (parse-bin co2-rating))) ; 482500
