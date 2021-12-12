(ns zelark.aoc-2021.day-12
  (:require [zelark.aoc.core :as aoc]
            [clojure.string :as str]))

;; --- Day 12: Passage Pathing ---
;; https://adventofcode.com/2021/day/12

(def input (aoc/get-input 2021 12))
     
(defn parse-input [input]
  (let [parsed (->> (str/split-lines input)
                    (map #(str/split % #"-")))]
    (merge-with into
     (-> (group-by first parsed)
         (update-vals #(mapv second %)))
     (-> (group-by second parsed)
         (update-vals #(mapv first %))))))

;; part 1
(defn find-paths [g path end]
  (let [start   (peek path)
        visited (remove #(re-find #"^[A-Z]+$" %) path)
        found   (cond->> (g start) (seq visited) (remove (set visited)))]
    (cond
      (= start end) [path]
      (seq found) (mapcat #(find-paths g (conj path %) end) found)
      :else nil)))

(count (find-paths (parse-input input) ["start"] "end")) ; 5252

;; part 2
(defn find-paths-2 [g path end]
  (let [start (peek path)
        visited (remove #(re-find #"^[A-Z]+$" %) path)
        some-small-visited-twice? (some #{2} (-> (frequencies visited)
                                                 (dissoc "start")
                                                 (vals)))
        visited (if some-small-visited-twice? visited #{"start"})
        found (cond->> (g start) (seq visited) (remove (set visited)))]
    (cond
      (= start end) [path]
      (seq found) (mapcat #(find-paths-2 g (conj path %) end) found)
      :else nil)))

(count (find-paths-2 (parse-input input) ["start"] "end")) ; 147784

