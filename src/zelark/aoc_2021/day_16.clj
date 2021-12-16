(ns zelark.aoc-2021.day-16
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]
            [clojure.walk :as walk]))

;; --- Day 16: Packet Decoder ---
;; https://adventofcode.com/2021/day/16

(def input (aoc/get-input 2021 16))

(defn parse-input [input]
  (str/escape input
              {\0 "0000" \1 "0001" \2 "0010" \3 "0011"
               \4 "0100" \5 "0101" \6 "0110" \7 "0111"
               \8 "1000" \9 "1001" \A "1010" \B "1011"
               \C "1100" \D "1101" \E "1110" \F "1111"}))

(defn parse-bin [s]
  (Long/parseLong s 2))

(defn read-trn! [buf n]
  (let [trn (deref buf)
        ret (subs trn 0 n)
        _   (reset! buf (subs trn n))]
    ret))

(defn read-header [trn]
  {:version (parse-bin (read-trn! trn 3))
   :type-id (parse-bin (read-trn! trn 3))})

(defn read-groups [trn]
  (loop [bin ""]
    (if (zero? (parse-bin (read-trn! trn 1)))
      (parse-bin (str bin (read-trn! trn 4)))
      (recur (str bin (read-trn! trn 4))))))

(defn read-packet [trn]
  (let [{:keys [type-id] :as packet} (read-header trn)]
    (if (== type-id 4)
      (assoc packet :number (read-groups trn))
      (let [length-type-id (parse-bin (read-trn! trn 1))]
        (case length-type-id
          0 (let [number-of-bits (parse-bin (read-trn! trn 15))]
              (assoc packet
                     :sub-packets
                     (let [bits   (read-trn! trn number-of-bits)
                           trn-in (atom bits)]
                       (loop [sub-packets []]
                         (if (seq @trn-in)
                           (recur (conj sub-packets (read-packet trn-in)))
                           sub-packets)))))
          1 (let [number-of-sub-packets (parse-bin (read-trn! trn 11))]
              (assoc packet
                     :sub-packets
                     (doall (repeatedly number-of-sub-packets #(read-packet trn))))))))))

;; part 1
(let [trn (atom (parse-input input))
      acc (atom 0)]
  (walk/postwalk (fn [x]
                   (when (map? x)
                     (swap! acc + (:version x)))
                   x)
                 (read-packet trn))
  @acc) ; 920

;; part 2
(defn type->op [t]
  (case t
    0 '+
    1 '*
    2 'min
    3 'max
    5 (fn gt [a b] (if (> a b) 1 0))
    6 (fn lt [a b] (if (< a b) 1 0))
    7 (fn eq [a b] (if (== a b) 1 0))))

(defn read-packet-2 [trn]
  (let [{:keys [type-id] :as packet} (read-header trn)]
    (if (== type-id 4)
      (read-groups trn)
      (let [length-type-id (parse-bin (read-trn! trn 1))]
        (case length-type-id
          0 (let [number-of-bits (parse-bin (read-trn! trn 15))]
              (apply list
                     (type->op type-id)
                     (let [bits   (read-trn! trn number-of-bits)
                           trn-in (atom bits)]
                       (loop [sub-packets []]
                         (if (seq @trn-in)
                           (recur (conj sub-packets (read-packet-2 trn-in)))
                           sub-packets)))))
          1 (let [number-of-sub-packets (parse-bin (read-trn! trn 11))]
              (apply list
                     (type->op type-id)
                     (repeatedly number-of-sub-packets #(read-packet-2 trn)))))))))

(let [trn (atom (parse-input input))]
  (eval (read-packet-2 trn))) ; 10185143721112
