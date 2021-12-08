(ns zelark.aoc-2021.day-08
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.set :as set]))

;; --- Day 8: Seven Segment Search ---
;; https://adventofcode.com/2021/day/8

(def input (aoc/get-input 2021 8))

(defn parse-entry [e]
  (let [[patterns digits] (->> (re-seq #"[abcdefg]+" e)
                               (split-at 10))]
    [(set (map set patterns))
     (mapv set digits)]))

 (defn parse-input [input]
   (->> (str/split-lines input)
        (map parse-entry)))

;; part 1
(->> (parse-input input)
     (map #(->> % second (map count)))
     (flatten)
     (filter #{2 3 4 7})
     (count)) ; 532

;; part 2
(defn find-1478 [ps]
  (reduce (fn [m p]
            (condp == (count p)
              2 (assoc m 1 p)
              3 (assoc m 7 p)
              4 (assoc m 4 p)
              7 (assoc m 8 p)
              m))
          {}
          ps))

(defn find-9 [m ps]
  (let [[nine] (filter #(set/subset? (m 4) %) ps)]
    (assoc m 9 nine)))

(defn find-03 [m ps]
  (let [[three zero] (->> (filter #(set/subset? (m 1) %) ps)
                          (sort-by count))]
    (assoc m
           0 zero
           3 three)))

(defn find-6 [m ps]
  (let [[six] (filter #(== (count %) 6) ps)]
    (assoc m 6 six)))

(defn find-25 [m ps]
  (let [seg  (set/difference (m 8) (m 9))
        two  (first (filter #(set/subset? seg %) ps))
        five (first (disj ps two))]
    (assoc m
           2 two
           5 five)))

(defn calc-entry [entry]
  (let [[patterns digits] entry
        mapping  (find-1478 patterns)
        patterns (apply disj patterns (vals mapping))
        mapping  (find-9 mapping patterns)
        patterns (apply disj patterns (vals mapping))
        mapping  (find-03 mapping patterns)
        patterns (apply disj patterns (vals mapping))
        mapping  (find-6 mapping patterns)
        patterns (apply disj patterns (vals mapping))
        mapping  (find-25 mapping patterns)
        mapping  (set/map-invert mapping)]
    (reduce #(+ (* %1 10) (mapping %2)) 0 digits)))

(->> (parse-input input)
     (map calc-entry)
     (reduce +)) ; 1011284