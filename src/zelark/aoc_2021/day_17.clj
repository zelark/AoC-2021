(ns zelark.aoc-2021.day-17
  (:require [zelark.aoc.core :as aoc]))

;; --- Day 17: Trick Shot ---
;; https://adventofcode.com/2021/day/17

(def input (aoc/get-input 2021 17))
        
(defn parse-input [input]
  (->> (re-seq #"(-?\d+)..(-?\d+)" input)
       (mapv #(mapv parse-long (rest %)))))

(defn step [{[vx vy] :vel :as state}]
  (-> state
      (update-in [:pos 0] + vx)
      (update-in [:pos 1] + vy)
      (update-in [:vel 0] + (compare 0 vx))
      (update-in [:vel 1] - 1)))

(defn within? [[[min-x max-x] [min-y max-y]] {[x y] :pos}]
  (and (<= min-x x max-x)
       (<= min-y y max-y)))

(defn pass? [[[_ max-x] [min-y]] {[x y] :pos}]
  (or (> x max-x)
      (< y min-y)))

(defn calc-trajectory [target-area [vx vy]]
  (let [init-state {:pos [0 0] :vel [vx vy]}
        steps (iterate step init-state)
        makes-sense? (fn [state]
                       (not (or (within? target-area state)
                                (pass? target-area state))))
        state (first (drop-while makes-sense? steps))]
    (when (within? target-area state)
      (->> (take-while #(not (within? target-area %)) steps)
           (map :pos)))))

(defn calc-trajectories [target-area velocity-range]
  (->> (for [vx velocity-range
             vy velocity-range]
         (calc-trajectory target-area [vx vy]))
       (remove nil?)))

;; part 1
(let [target-area (parse-input input)]
  (->> (calc-trajectories target-area (range 1 100))
       (mapcat #(map second %))
       (apply max))) ; 4278

;; part 2
(let [target-area (parse-input input)]
  (->> (calc-trajectories target-area (range -250 250))
       (count))) ; 1994
