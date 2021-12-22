(ns zelark.aoc.core
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-input [year day]
  (-> (format "%d/input_%02d.txt" year day)
      (io/resource)
      (slurp)
      (str/trim-newline)))

;; Parsing
(defn parse-longs [s]
  (->> (re-seq #"-?\d+" s)
       (mapv parse-long)))

(defn parse-bin [s]
  (Long/parseLong s 2))

;; Math
(defn mod-1
  "Returns the 1-based modulus `base` of `n`"
  [n base]
  (inc (mod (dec n) base)))

(defn rangex
  ([] ())
  ([start end]
   (if (<= start end)
     (range start (inc end))
     (range start (dec end) -1))))

;; Grids
(defn empty-grid [w h]
  (vec (repeat h (vec (repeat w \.)))))

(defn print-grid [grid]
  (->> (map str/join grid)
       (str/join \newline)
       (println)))

(defn mark-point [grid [x y] v]
  (assoc-in grid [y x] v))

(defn print-points [points]
  (let [max-x (apply max (map first points))
        max-y (apply max (map second points))]
    (-> (reduce #(mark-point %1 %2 \#)
                (empty-grid (inc max-x) (inc max-y))
                points)
        (print-grid))))
