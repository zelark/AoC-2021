(ns zelark.aoc-2021.day-22
  (:require [clojure.string :as str]
            [zelark.aoc.core :as aoc]))

;; --- Day 22: Reactor Reboot ---
;; https://adventofcode.com/2021/day/22

(def input (aoc/get-input 2021 22))

(defn parse-entry [entry]
  (let [[state coords] (str/split entry #"\s")]
    [(keyword state)
     (partition 2 (aoc/parse-longs coords))]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map parse-entry)))

(defn length [[start end]]
  (inc (- end start)))

(defn overlap [[start-a end-a] [start-b end-b]]
  (when (and (<= start-a end-b)
             (<= start-b end-a))
    [(max start-a start-b) (min end-a end-b)]))

(defn clip [steps windows]
  (keep (fn [[state ranges]]
          (let [os (map overlap ranges windows)]
            (when (every? seq os)
              [state os])))
        steps))

(defn calc-cubes [[_ ranges] steps]
  (loop [result   (reduce * (map length ranges))
         overlaps (clip steps ranges)]
    (if-let [overlap (first overlaps)]
      (recur (- result (calc-cubes overlap (next overlaps)))
             (next overlaps))
      result)))

(defn solve
  ([input]
   (solve input nil))
  ([input window]
   (loop [result 0
          steps  (cond-> (parse-input input)
                   window (clip (repeat 3 window)))]
     (let [[state _ :as step] (first steps)]
       (cond
         (nil? step)    result
         (= state :off) (recur result (next steps))
         :else          (recur (+ result (calc-cubes step (next steps)))
                               (next steps)))))))

;; part 1
(solve input [-50 50]) ; 503864

;; part 2
(solve input nil) ; 1255547543528356
