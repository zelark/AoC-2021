(ns zelark.aoc-2021.day-24
  (:require [clojure.string :as str]
            [clojure.edn :as edn]
            [zelark.aoc.core :as aoc]))

;; --- Day 24: Arithmetic Logic Unit ---
;; https://adventofcode.com/2021/day/24

(def input (aoc/get-input 2021 24))

(defn parse-input [input]
  (->> (str/split-lines input)
       (mapv #(str "[" % "]"))
       (mapv edn/read-string)
       (mapv (fn [x] (mapv #(cond-> % (symbol? %) keyword) x)))))

;; Code for Arithmetic Logic Unit.
;; Surprise, but to solve the problem we don't need it at all.
(defn halt? [op]
  (= op :hlt))

(defn update-state [state [op a b]]
  (let [va (state a a)
        vb (state b b)]
    (case op
      :inp (if-let [[value & remain-input] (:input state)]
             (-> state
                 (assoc a value)
                 (assoc :input remain-input))
             (throw (ex-info "No input!" state)))
      :add (assoc state a (+ va vb))
      :mul (assoc state a (* va vb))
      :div (assoc state a (int (/ va vb)))
      :mod (assoc state a (mod va vb))
      :eql (assoc state a (if (== va vb) 1 0)))))

(defn run-alu [code init]
  (loop [state init, ip 0]
    (let [[op :as command] (nth code ip [:hlt])]
      (if (halt? op)
        state
        (recur (update-state state command) (inc ip))))))

(defn valid-model-number? [monad model-number]
  (let [input (->> model-number str (map #(Character/digit % 10)))]
    (-> (run-alu monad {:w 0 :x 0 :y 0 :z 0 :input input})
        :z zero?)))

;; Actual solution
(defn analize-monad [monad]
  (loop [xs (->> (partition 18 monad)
                 (map #(-> [(peek (nth % 5)) (peek (nth % 15))])))
         result (sorted-map)
         i 0
         stack []]
    (if-let [[a b] (first xs)]
      (if (pos? a)
        (recur (next xs)
               result
               (inc i)
               (conj stack [i b]))
        (recur (next xs)
               (let [[j offset :as v] (update (peek stack) 1 + a)]
                 (if (pos? offset)
                   (assoc result i v)
                   (assoc result j [i (- offset)])))
               (inc i)
               (pop stack)))
      result)))

(defn find-number [rules mapping]
  (->> (reduce-kv mapping (sorted-map) rules)
       (vals)
       (str/join)
       (parse-long)))

;; MOdel Number Automatic Detector program
(def monad (parse-input input))
(def rules (analize-monad monad))

;; part 1
(find-number rules
             (fn [m i [j offset]]
               (assoc m
                      i 9
                      j (- 9 offset)))) ; 74929995999389

(valid-model-number? monad 74929995999389)

;; part 2
(find-number rules
             (fn [m i [j offset]]
               (assoc m
                      i (+ 1 offset)
                      j 1))) ; 11118151637112

(valid-model-number? monad 11118151637112)
