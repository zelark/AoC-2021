(ns zelark.aoc-2021.day-21
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 21: Dirac Dice ---
;; https://adventofcode.com/2021/day/21

(def input (aoc/get-input 2021 21))

(defn parse-input [input]
  (->> (re-seq #"(Player \d).+(\d+)" input)
       (mapv (fn [[_ name position]]
               {:name name
                :position (parse-long position)
                :score 0}))))

(defn move [position steps]
  (aoc/mod-1 (+ position steps) 10))

;; part 1
(defn play [state]
  (loop [[current another] state
         dice (cycle (range 1 101))
         n 0]
    (let [position' (move (:position current) (apply + (take 3 dice)))
          current' (-> current
                       (assoc :position position')
                       (update :score + position'))]
      (if (>= (:score current') 1000)
        (* (:score another) (+ n 3))
        (recur [another current'] (drop 3 dice) (+ n 3))))))

(play (parse-input input)) ; 707784

;; part 2
(def play-hard
  (memoize
   (fn [[current another]]
     (->> (for [[steps freq] [[3 1] [4 3] [5 6] [6 7] [7 6] [8 3] [9 1]]]
            (let [position' (move (current :position) steps)
                  current'  (-> current
                                (assoc :position position')
                                (update :score + position'))]
              (if (>= (:score current') 21)
                {(:name current) freq}
                (-> (play-hard [another current'])
                    (update-vals #(* % freq))))))
          (apply merge-with +)))))

(time (->> (play-hard (parse-input input))
           (vals)
           (apply max))) ; 157595953724471
