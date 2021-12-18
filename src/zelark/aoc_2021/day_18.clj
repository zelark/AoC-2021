(ns zelark.aoc-2021.day-18
  (:require [clojure.edn :as edn]
            [clojure.string :as str]
            [clojure.java.math :as math]
            [clojure.zip :as zip]
            [zelark.aoc.core :as aoc]))

;; --- Day 18: Snailfish ---
;; https://adventofcode.com/2021/day/18

(def input (aoc/get-input 2021 18))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map edn/read-string)))

;; zipper helpers
(def depth (comp count zip/path))

(defn find-loc
  ([zipper pred]
   (find-loc zipper pred zip/next))
  ([zipper pred move]
   (loop [loc (move zipper)]
     (when-not (or (nil? loc) (zip/end? loc))
       (if (pred loc)
         loc
         (recur (move loc)))))))

;; solution
(def leaf? (comp number? zip/node))

(defn next-leaf [loc]
  (find-loc loc leaf?))

(defn prev-leaf [loc]
  (find-loc loc leaf? zip/prev))

(defn explode [loc]
  (let [update-prev (fn [loc n]
                      (if-let [prev (prev-leaf loc)]
                        (next-leaf (zip/edit prev + n))
                        loc))
        update-next (fn [loc n]
                      (if-let [next (next-leaf loc)]
                        (prev-leaf (zip/edit next + n))
                        loc))
        [a b] (zip/node loc)]
    (-> (zip/replace loc 0)
        (update-prev a)
        (update-next b)
        (zip/root))))

(defn split [loc]
  (let [n (/ (zip/node loc) 2)]
    (-> loc
        (zip/replace [(int (math/floor n))
                      (int (math/ceil n))])
        (zip/root))))

(defn nested-pair? [loc]
  (and (== (depth loc) 4)
       (zip/branch? loc)))

(defn regular-ten? [loc]
  (let [x (zip/node loc)]
    (and (number? x)
         (>= x 10))))

(defn reduce-fish-number [fish-number]
  (condp #(find-loc %2 %1) (zip/vector-zip fish-number)
    nested-pair? :>> explode
    regular-ten? :>> split
    fish-number))

(defn add-fish-numbers [a b]
  (->> [a b]
       (iterate reduce-fish-number)
       (reduce #(if (= %1 %2) (reduced %1) %2))))

(defn calc-magnitude [fish-number]
  (if (number? fish-number)
    fish-number
    (+ (* 3 (calc-magnitude (first fish-number)))
       (* 2 (calc-magnitude (second fish-number))))))

;; part 1
(->> (parse-input input)
     (reduce add-fish-numbers)
     (calc-magnitude)) ; 4033

;; part 2
(let [fish-numbers (parse-input input)]
  (->> (for [a fish-numbers
             b fish-numbers
             :when (not= a b)]
         (add-fish-numbers a b))
       (map calc-magnitude)
       (apply max))) ; 4864
