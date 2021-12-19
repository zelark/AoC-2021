(ns zelark.aoc-2021.day-19
  (:require [clojure.string :as str]
            [clojure.math.combinatorics :as combo]
            [zelark.aoc.core :as aoc]))

;; --- Day 19: Beacon Scanner ---
;; https://adventofcode.com/2021/day/19

(def input (aoc/get-input 2021 19))

(defn parse-scanner [s]
  (let [[_name & bs] (str/split-lines s)]
    (mapv aoc/parse-longs bs)))

(defn parse-input [input]
  (->> (str/split input #"\R\R")
       (map parse-scanner)))

;; orientations for x axis
;; [+x +y +z]
;; [+x -z +y]
;; [+x -y -z]
;; [+x +z -y]
;; [-x +y -z]
;; [-x +z +y]
;; [-x -y +z]
;; [-x -z -y]

(defn calc-orientations [beacons]
  (concat
   (for [[x y z] [[0 1 2] [1 2 0] [2 0 1]]
         [dx dy dz] [[+ + +] [+ - -] [- + -] [- - +]]]
     (mapv (fn [b] [(dx (b x)) (dy (b y)) (dz (b z))]) beacons))
   (for [[x y z] [[0 2 1] [1 0 2] [2 1 0]]
         [dx dy dz] [[+ - +] [+ + -] [- + +] [- - -]]]
     (mapv (fn [b] [(dx (b x)) (dy (b y)) (dz (b z))]) beacons))))

(defn overlap [s1 s2]
  (->> (for [d1 s1 d2 s2] (map - d1 d2))
       (group-by identity)
       (vals)
       (some #(when (>= (count %) 12)
                (first %)))))

(defn add-offset [beacons offset]
  (map #(mapv + offset %) beacons))

(defn find-overlapping [{base :beacons} scanners]
  (keep (fn [beacons]
          (some #(when-let [offset (overlap base %)]
                   (let [with-offset (add-offset % offset)]
                     {:offset offset
                      :beacons with-offset
                      :origin beacons}))
                (calc-orientations beacons)))
        scanners))

(defn solve [input]
  (let [[zero & scanners] (parse-input input)
        scanners (set scanners)]
    (loop [scan-map [{:beacons zero
                      :offset [0 0 0]}]
           queue    #{{:beacons zero}}
           scanners (set scanners)]
      (if-let [current (first queue)]
        (if-let [found (find-overlapping current scanners)]
          (recur (into scan-map found)
                 (into (disj queue current) found)
                 (reduce disj scanners (map :origin found)))
          (recur scan-map (disj queue current) scanners))
        scan-map))))

;; part 1
(->> (solve input)
     (mapcat :beacons)
     (set)
     (count)) ; 394

;; part 2
(defn manhattan-distance [[x1 y1 z1] [x2 y2 z2]]
  (+ (Math/abs (- x2 x1))
     (Math/abs (- y2 y1))
     (Math/abs (- z2 z1))))

(let [scans (->> (solve input)
                 (map :offset))]
  (->> (combo/combinations scans 2)
       (map (fn [[a b]] (manhattan-distance a b)))
       (apply max))) ; 12304
