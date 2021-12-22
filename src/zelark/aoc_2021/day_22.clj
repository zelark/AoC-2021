(ns zelark.aoc-2021.day-22
  (:require [clojure.java.math :as math]
            [clojure.string :as str]
            [clojure.set :as set]
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

(defn subrange [[start end] low high]
  (cond
    (< high start) []
    (< end low)    []
    :else          [(min (max start low) high)
                    (min (max end low) high)]))

;; part 1
(defn calc-cuboids [[xr yr zr]]
  (set (for [x (apply aoc/rangex (subrange xr -50 50))
             y (apply aoc/rangex (subrange yr -50 50))
             z (apply aoc/rangex (subrange zr -50 50))]
         [x y z])))

(->> (parse-input input)
     (reduce (fn [acc [state coords]]
               (let [cuboids (calc-cuboids coords)]
                 (case state
                   :on  (into acc cuboids)
                   :off (set/difference acc cuboids))))
             #{})
     (count)) ; 503864

;; part 2
(defn calc-total [[[x1 x2] [y1 y2] [z1 z2]]]
  (* (inc (math/abs (- x1 x2)))
     (inc (math/abs (- y1 y2)))
     (inc (math/abs (- z1 z2)))))
    
(defn calc-total-untouched [[_ current] rest]
  (let [total   (calc-total current)
        touched (keep (fn [[state other]]
                        (let [ss (map #(apply subrange %1 %2) other current)]
                          (when-not (some empty? ss)
                            [state ss]))) rest)]
    (loop [total   total
           touched touched]
      (if-let [item (first touched)]
        (recur (- total (calc-total-untouched item (next touched)))
               (next touched))
        total))))
    
(loop [result 0
       steps  (parse-input input)]
  (if-let [step (first steps)]
    (if (= (first step) :off)
      (recur result (next steps))
      (recur (+ result (calc-total-untouched step (next steps)))
             (next steps)))
    result)) ; 1255547543528356
