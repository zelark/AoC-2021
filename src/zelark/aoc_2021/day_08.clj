(ns zelark.aoc-2021.day-08
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 8: Seven Segment Search ---
;; https://adventofcode.com/2021/day/8

(def input (aoc/get-input 2021 8))

(defn parse-entry [entry]
  (->> (re-seq #"[abcdefg]+" entry)
       (split-at 10)))

 (defn parse-input [input]
   (->> (str/split-lines input)
        (map parse-entry)))

(defn unique? [code]
  (contains? #{2 3 4 7} (count code)))

;; part 1
(->> (parse-input input)
     (mapcat second)
     (filter unique?)
     (count)) ; 532

;; part 2
(def segments->digit
  {#{\a \b \c \e \f \g}    0
   #{\c \f}                1
   #{\a \c \d \e \g}       2
   #{\a \c \d \f \g}       3
   #{\b \c \d \f}          4
   #{\a \b \d \f \g}       5
   #{\a \b \d \e \f \g}    6
   #{\a \c \f}             7
   #{\a \b \c \d \e \f \g} 8
   #{\a \b \c \d \f \g}    9})

(def match->segment
  {[2 6] \a
   [2 4] \b
   [4 4] \c
   [2 5] \d
   [1 3] \e
   [4 5] \f
   [1 6] \g})

(defn decode-digit [mapping digit]
  (get segments->digit
       (set (map mapping digit))))

(defn decode-entry [[patterns digits]]
  (let [{unique true others false} (group-by unique? patterns)
        freqs1 (frequencies (str/join unique))
        freqs2 (frequencies (str/join others))
        mapping (reduce-kv (fn [m seg n]
                             (assoc m seg (match->segment [n (freqs2 seg)])))
                           {}
                           freqs1)]
    (->> (map #(decode-digit mapping %) digits)
         (str/join)
         (parse-long))))

(->> (parse-input input)
     (map decode-entry)
     (reduce +)) ; 1011284
