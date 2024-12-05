(ns dvliman.2024.day04
  (:require [clojure.java.io :as io]))

(def directions [[1 0]   ;; south
                 [1 1]   ;; south east (right)
                 [1 -1]  ;; south west (left)
                 [-1 0]  ;; north
                 [-1 1]  ;; north east
                 [-1 -1] ;; north west
                 [0 1]   ;; east
                 [0 -1]  ;; west
                 ])

(defn check-direction [[dx dy] [row col] grid]
  (map (partial get-in grid)
       (take 4 (iterate (fn [[row col]]
                          [(+ row dx) (+ col dy)]) [row col]))))

(->> "2024/day04.txt"
     io/resource
     io/reader
     line-seq
     (map vec)
     vec
     ((fn process' [input]
        (->> (for [x (range (count (first input)))
                   y (range (count input))]
               (for [direction directions]
                 (check-direction direction [x y] input)))
             (mapcat identity)
             (filter #(= % '(\X \M \A \S)))
             count))))
;; => 2344

(defn check-x-direction [[dx dy] [row col] grid]
  (map (partial get-in grid)
       (take 3 (iterate (fn [[row col]]
                          [(+ row dx) (+ col dy)]) [row col]))))

(->> "2024/day04.txt"
     io/resource
     io/reader
     line-seq
     (map vec)
     vec
     ((fn process' [input]
        (->> (for [row (range (count input))
                   col (range (count (first input)))]
               (let [se (check-x-direction [1 1] [row (dec col)] input)
                     ne (check-x-direction [1 -1] [row (inc col)] input)]
                 (cond
                   (and (or (= se '(\S \A \M)) (= se '(\M \A \S)))
                        (or (= ne '(\S \A \M)) (= ne '(\M \A \S))))
                   :match
                   :else nil)))
             (filter some?)
             count))))
;; => 1815
