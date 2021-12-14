(ns zelark.aoc-2021.day-14
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 14: Extended Polymerization ---
;; https://adventofcode.com/2021/day/14

(def input (aoc/get-input 2021 14))
        
(defn parse-input [input]
  (let [[template rules] (str/split input #"\R\R")]
    {:template template
     :rules (->> (re-seq #"(.*) -> (.*)" rules)
                 (map (fn [[_ [a b] [c]]] [[a b] c]))
                 (into {}))}))

(defn step [pairs rules]
  (reduce-kv (fn [m [a b :as p] v]
               (if-let [c (get rules p)]
                 (-> m
                     (update [a c] (fnil + 0) v)
                     (update [c b] (fnil + 0) v))
                 m)) {} pairs))

(defn solve [input n]
  (let [{:keys [template rules]} (parse-input input)
        freqs (->> (partition 2 1 template)
                   (frequencies))
        freqs' (->> (iterate #(step % rules) freqs)
                    (drop n)
                    (first)
                    (reduce-kv (fn [m [a b] v]
                                 (-> m
                                     (update a (fnil + 0) v)
                                     (update b (fnil + 0) v))) {}))]
    (inc (- (quot (apply max (vals freqs')) 2)
            (quot (apply min (vals freqs')) 2)))))

;; part 1
(solve input 10) ; 2899

;; part 2
(solve input 40) ; 3528317079545
