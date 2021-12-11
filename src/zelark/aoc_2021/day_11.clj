(ns zelark.aoc-2021.day-11
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 11: Dumbo Octopus ---
;; https://adventofcode.com/2021/day/11

(def input (aoc/get-input 2021 11))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapcat (fn [y line] (keep-indexed (fn [x n] (-> [[x y] (Character/digit n 10)])) line)) (range))
       (into {})))

(defn adjacent [[x y]]
  (for [dx [-1 0 1] dy [-1 0 1] :when (not= dx dy 0)]
    [(+ x dx) (+ y dy)]))

(defn flash [state flashed current]
  (let [adjacent-octopuses (->> (adjacent current)
                                (remove flashed))]
    (merge state
           (-> (select-keys state adjacent-octopuses)
               (update-vals inc)
               (update current :assoc 0)))))

(defn find-full [full flashed state]
  (reduce-kv (fn [f k v]
               (if (and (not (flashed k))
                        (>= v 10))
                 (conj f k)
                 f))
             full
             state))

(defn step [[state n]]
  (loop [state (update-vals state inc)
         flashed #{}
         full (find-full #{} flashed state)]
    (if-let [current (first full)]
      (let [state'   (flash state flashed current)
            flashed' (conj flashed current)
            full'    (-> (disj full current)
                         (find-full flashed' state'))]
        (recur state' flashed' full'))
      [state (+ n (count flashed))])))

;; part 1
(->> [(parse-input input) 0]
     (iterate step)
     (drop 100)
     (first)
     (second)) ; 1719


;; part 2
(defn async? [[state]]
  (some pos? (vals state)))

(->> [(parse-input input) 0]
     (iterate step)
     (take-while async?)
     (count)) ; 232
