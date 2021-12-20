(ns zelark.aoc-2021.day-20
  (:require [clojure.string :as str]
            [zelark.aoc.core :as aoc]))

;; --- Day 20: Trench Map ---
;; https://adventofcode.com/2021/day/20

(def input (aoc/get-input 2021 20))

(def pixel->digit {\# \1 \. \0})

(defn parse-input-image [input]
  (->> (str/split-lines input)
       (mapcat (fn [y line] (keep-indexed (fn [x p] [[x y] (pixel->digit p)]) line)) (range))
       (into {})))

(defn parse-input [input]
  (let [[algorithm input-image] (str/split input #"\R\R")]
    {:algorithm (vec (replace pixel->digit algorithm))
     :image (parse-input-image input-image)}))

(defn adjacent+ [[x y]]
  (for [dy [-1 0 1] dx [-1 0 1]]
    [(+ x dx) (+ y dy)]))

(defn enhance [image algo void-pixel]
  (let [min-x (apply min (map first (keys image)))
        min-y (apply min (map second (keys image)))
        max-x (apply max (map first (keys image)))
        max-y (apply max (map second (keys image)))
        extra 1]
    (reduce (fn [output pixel]
              (let [i (-> (reduce #(str %1 (get image %2 void-pixel)) "" (adjacent+ pixel))
                          (aoc/parse-bin))]
                (assoc output pixel (nth algo i))))
            {}
            (for [x (range (- min-x extra) (inc (+ max-x extra)))
                  y (range (- min-y extra) (inc (+ max-y extra)))]
              [x y]))))

(defn solve [input n]
  (let [{:keys [algorithm image]} (parse-input input)]
    (loop [image image
           void (if (= (nth algorithm 0) \1)
                  (cycle [0 1]) ; for real input
                  (cycle [0]))  ; for demo input
           n n]
      (if (zero? n)
        (->> image vals (filter #{\1}) count)
        (recur (enhance image algorithm (first void))
               (next void)
               (dec n))))))

;; part 1
(solve input 2) ; 5622

;; part 2
(solve input 50) ; 20395
