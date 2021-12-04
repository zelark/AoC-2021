(ns zelark.aoc-2021.day-04
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 4: Giant Squid ---
;; https://adventofcode.com/2021/day/4

(def input (aoc/get-input 2021 04))

(defn parse-numbers [xs]
  (->> (str/split xs #",|\s")
       (remove str/blank?)
       (mapv parse-long)))

(defn parse-board [xs]
  (let [board (mapv parse-numbers (next xs))]
    {:original board
     :rotated  (apply mapv vector board)}))

(defn parse-input [input]
  (let [[numbers & boards] (str/split-lines input)]
    {:numbers (parse-numbers numbers)
     :boards  (set (map parse-board (partition 6 boards)))}))

(defn winner [drawn-numbers {:keys [original rotated] :as board}]
  (when (or (some #(every? drawn-numbers %) original)
            (some #(every? drawn-numbers %) rotated))
    board))

(defn score [drawn-numbers n {board :original}]
  (->> (reduce into board)
       (remove drawn-numbers)
       (reduce +)
       (* n)))

;; part 1
(let [{:keys [numbers boards]} (parse-input input)]
  (reduce (fn [nums n]
            (let [nums' (conj nums n)
                  board (some #(winner nums' %) boards)]
              (if board
                (reduced (score nums' n board))
                nums')))
          (set (take 4 numbers))
          (drop 4 numbers))) ; 35670

;; part 2
(let [{:keys [numbers boards]} (parse-input input)]
  (-> (reduce (fn [[drawn-numbers left-boards winning-boards] n]
                (let [drawn-numbers' (conj drawn-numbers n)
                      winners (filter #(winner drawn-numbers' %) left-boards)]
                  (if (seq winners)
                    [drawn-numbers'
                     (reduce disj left-boards winners)
                     (into winning-boards
                           (map #(score drawn-numbers' n %) winners))]
                    [drawn-numbers' left-boards winning-boards])))
              [(set (take 4 numbers)) boards []]
              (drop 4 numbers))
      (peek)
      (peek))) ; 22704
