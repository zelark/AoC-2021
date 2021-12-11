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

(defn flash [state flashed full]
  (let [update-state (fn [m k v] (cond-> m (contains? m k) (update k + v)))
        affected (->> (mapcat adjacent full)
                      (remove flashed)
                      (frequencies)
                      (reduce-kv update-state state))]
    (merge affected (zipmap full (repeat 0)))))

(defn find-full [state]
  (->> (filter #(<= 10 (second %)) state)
       (keys)))

(defn step [[state n]]
  (loop [state (update-vals state inc)
         flashed #{}]
    (if-let [full (seq (find-full state))]
      (recur (flash state flashed full)
             (into flashed full))
      [state (+ n (count flashed))])))

(def steps (iterate step [(parse-input input) 0]))

;; part 1
(second (nth steps 100)) ; 1719

;; part 2
(defn async? [[state]]
  (some pos? (vals state)))

(count (take-while async? steps)) ; 232
