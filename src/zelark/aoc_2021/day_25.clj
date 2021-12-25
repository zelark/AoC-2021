(ns zelark.aoc-2021.day-25
  (:require [clojure.string :as str]
            [zelark.aoc.core :as aoc]))

;; --- Day 25: Sea Cucumber ---
;; https://adventofcode.com/2021/day/25

(def input (aoc/get-input 2021 25))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapcat (fn [y line] (keep-indexed (fn [x v] (-> [[x y] v])) line)) (range))
       (into {})))

(defn next-x [mx [x y]]
  [(if (== x mx) 0 (inc x)) y])

(defn next-y [my [x y]]
  [x (if (== y my) 0 (inc y))])

;; part 1
(defn step [world herd next]
  (reduce (fn [herd loc]
            (let [nloc (next loc)]
              (if (some #(% nloc) world)
                (conj herd loc)
                (conj herd nloc))))
          #{}
          herd))

(defn step-to-west [{:keys [west-herd] :as state} nwest]
  (assoc state :west-herd (step (vals state) west-herd nwest)))

(defn step-to-south [{:keys [south-herd] :as state} nsouth]
  (assoc state :south-herd (step (vals state) south-herd nsouth)))

(let [cucumbers (parse-input input)
      max-x (apply max (map first (keys cucumbers)))
      max-y (apply max (map second (keys cucumbers)))
      nx (partial next-x max-x)
      ny (partial next-y max-y)
      {south-herd \v west-herd \>} (group-by val cucumbers)]
  (->> {:west-herd  (set (keys west-herd))
        :south-herd (set (keys south-herd))}
       (iterate #(-> % (step-to-west nx) (step-to-south ny)))
       (partition 2)
       (take-while #(not= (first %) (second %)))
       (count)
       (* 2))) ; 598

;; part 2
;; As usual, just to have 49 stars to get the last one.
