(ns zelark.aoc-2021.day-03
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 3: Binary Diagnostic ---
;; https://adventofcode.com/2021/day/3

(def input (aoc/get-input 2021 03))

(defn parse-bin [s]
  (Long/parseLong s 2))

;; part 1
(let [gamma-rate (->> (str/split-lines input)
                      (apply map #(frequencies %&))
                      (map (partial sort-by (juxt second first)))
                      (map ffirst)
                      (apply str))
      epsilon-rate (str/escape gamma-rate {\0 \1 \1 \0})]
  (* (parse-bin gamma-rate)
     (parse-bin epsilon-rate))) ; 1307354

;; part 2
(defn most-common-bit [numbers n]
  (->> (map #(nth % n) numbers)
       (frequencies)
       (sort-by (juxt second first))
       ((comp first second))))

(defn least-common-bit [numbers n]
  (->> (map #(nth % n) numbers)
       (frequencies)
       (sort-by (juxt second first))
       (ffirst)))

(defn find-rating [numbers bit-fn]
  (-> (reduce (fn [acc i]
                (let [bit (bit-fn acc i)
                      ret (filter #(= (nth % i) bit) acc)]
                  (if (== (count ret) 1)
                    (reduced ret)
                    ret)))
              numbers
              (range (count (first numbers))))
      (first)))

(let [numbers (str/split-lines input)
      o2-rating (find-rating numbers most-common-bit)
      co2-rating (find-rating numbers least-common-bit)]
  (* (parse-bin o2-rating)
     (parse-bin co2-rating))) ; 482500

