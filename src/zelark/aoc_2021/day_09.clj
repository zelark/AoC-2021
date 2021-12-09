(ns zelark.aoc-2021.day-09
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 9: Smoke Basin ---
;; https://adventofcode.com/2021/day/9

(def input (aoc/get-input 2021 9))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapcat (fn [y line] (keep-indexed (fn [x n] (-> [[x y] (Character/digit n 10)])) line)) (range))
       (into {})))

(defn adjacent [[x y]]
  (for [[dx dy] [[0 -1] [0 1] [-1 0] [1 0]]]
    [(+ x dx) (+ y dy)]))

(defn low-point? [hmap [loc v]]
  (->> (keep hmap (adjacent loc))
       (every? #(< v %))))

(defn find-low-points [hmap]
  (filter (partial low-point? hmap) hmap))

;; part 1
(->> (parse-input input)
     (find-low-points)
     (vals)
     (map inc)
     (reduce +)) ; 452

;; part 2
(defn find-basin [hmap point]
  (loop [queue   #{point}
         visited #{}]
    (if-let [current (first queue)]
      (recur (->> (select-keys hmap (adjacent current))
                  (remove (comp #{9 nil} val))
                  (keys)
                  (remove visited)
                  (into (disj queue current)))
             (conj visited current))
      visited)))

(let [hmap (parse-input input)]
  (->> (find-low-points hmap)
       (keys)
       (map (partial find-basin hmap))
       (map count)
       (sort >)
       (take 3)
       (reduce *))) ; 1263735
