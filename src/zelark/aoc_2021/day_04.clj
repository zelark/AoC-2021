(ns zelark.aoc-2021.day-04
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 4: Giant Squid ---
;; https://adventofcode.com/2021/day/4

(def input (aoc/get-input 2021 04))

(defn parse-numbers [s]
  (as-> (str/trim s) $$
    (str/split $$ #",|\s+")
    (mapv parse-long $$)))

(defn parse-board [xs]
  (mapv parse-numbers (next xs)))

(defn parse-input [input]
  (let [[numbers & boards] (str/split-lines input)]
    {:numbers (parse-numbers numbers)
     :boards  (set (map parse-board (partition 6 boards)))}))

(defn winner? [drawn-numbers board]
  (or (some #(every? drawn-numbers %) board)
      (some #(every? drawn-numbers %)
            (apply mapv vector board))
      false))

(defn score [drawn-numbers n board]
  (->> (reduce into board)
       (remove drawn-numbers)
       (reduce +)
       (* n)))

(defn solve [input]
  (let [{:keys [numbers boards]} (parse-input input)]
    (-> (reduce (fn [[drawn-numbers left-boards winning-boards] n]
                  (let [drawn-numbers' (conj drawn-numbers n)
                        winners (filter #(winner? drawn-numbers' %) left-boards)]
                    (if (seq winners)
                      [drawn-numbers'
                       (reduce disj left-boards winners)
                       (into winning-boards
                             (map #(score drawn-numbers' n %) winners))]
                      [drawn-numbers' left-boards winning-boards])))
                [(set (take 4 numbers)) boards []]
                (drop 4 numbers))
        (peek))))

;; part 1
(first (solve input)) ; 35670

;; part 2
(peek (solve input)) ; 22704
