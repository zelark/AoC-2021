(ns zelark.aoc-2021.day-21
  (:require [clojure.math.combinatorics :as combo]
            [zelark.aoc.core :as aoc]))

;; --- Day 21: Dirac Dice ---
;; https://adventofcode.com/2021/day/21

(def input (aoc/get-input 2021 21))

(defn parse-input [input]
  (->> (re-seq #"(Player \d).+(\d+)" input)
       (mapv (fn [[_ name position]]
               {:name name
                :position (parse-long position)
                :score 0}))))

(defn move [p steps]
  (mod (+ p steps) 10))

(def score [10 1 2 3 4 5 6 7 8 9])

;; part 1
(defn play [state]
  (loop [[current another] state
         dice (cycle (range 1 101))
         n 0]
    (let [new-position (move (:position current) (apply + (take 3 dice)))
          current' (-> (assoc current :position new-position)
                       (update :score + (score new-position)))]
      (if (>= (:score current') 1000)
        (* (:score another) (+ n 3))
        (recur [another current'] (drop 3 dice) (+ n 3))))))

(play (parse-input input)) ; 707784

;; part 2
(def splits (->> (combo/selections [1 2 3] 3)
                 (map #(apply + %))
                 (frequencies)))

(def play-hard
  (memoize
   (fn [[current another]]
     (->> (for [[steps freq] splits]
            (let [new-position (move (current :position) steps)
                  current'     (-> (assoc current :position new-position)
                                   (update :score + (score new-position)))]
              (if (>= (:score current') 21)
                {(:name current) freq}
                (-> (play-hard [another current'])
                    (update-vals #(* % freq))))))
          (apply merge-with +)))))

(time (->> (play-hard (parse-input input))
           (vals)
           (apply max))) ; 157595953724471
