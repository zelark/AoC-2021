(ns zelark.aoc-2021.day-04
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 4: Giant Squid ---
;; https://adventofcode.com/2021/day/4

(def input (aoc/get-input 2021 04))

(defn parse-numbers [s]
  (->> (re-seq #"\d+" s)
       (map parse-long)))

 (defn parse-board [xs]
  (mapv parse-numbers (next xs)))
  
(defn parse-input [input]
  (let [[numbers & boards] (str/split-lines input)]
    {:numbers (parse-numbers numbers)
     :boards  (set (map parse-board (partition 6 boards)))}))

(defn winner? [drawn-numbers board]
  (->> (concat board (apply mapv vector board))
       (some #(every? drawn-numbers %))
       (boolean)))

(defn score [drawn-numbers last-drown-number board]
  (->> (flatten board)
       (remove drawn-numbers)
       (reduce +)
       (* last-drown-number)))

(defn solve [input]
  (let [{:keys [numbers boards]} (parse-input input)]
    (-> (reduce (fn [[drawn-numbers boards scores] n]
                  (let [drawn-numbers' (conj drawn-numbers n)
                        winners (filter #(winner? drawn-numbers' %) boards)]
                    (if (seq winners)
                      [drawn-numbers'
                       (reduce disj boards winners)
                       (into scores
                             (map #(score drawn-numbers' n %) winners))]
                      [drawn-numbers' boards scores])))
                [(set (take 4 numbers)) boards []]
                (drop 4 numbers))
        (peek))))

;; part 1
(first (solve input)) ; 35670
       
;; part 2
(peek (solve input)) ; 22704
