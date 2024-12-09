(ns dvliman.2024.day07
  (:require [clojure.java.io :as io]
            [clojure.math.combinatorics :as combo]))

(defn interleave-all
  [c1 c2]
  (lazy-seq
   (let [s1 (seq c1) s2 (seq c2)]
     (cond
       (and s1 s2)
       (cons (first s1) (cons (first s2)
                              (interleave-all (rest s1) (rest s2))))
       (some? s1) s1
       (some? s2) s2
       :else nil))))

(combo/selections ["+" "*"] 1)
(interleave)


(->> "2024/day07-example.txt"
     io/resource
     io/reader
     line-seq
     (map (partial re-seq #"\d+"))
     (map (partial map parse-long))
     (map (partial (fn [nums]
                     (let [test-value (first nums)
                           numbers    (rest nums)]
                       [test-value
                        numbers
                        (combo/selections ["+" "*"] (dec (count numbers)))
                        (reduce (fn [acc orders]
                                  (conj acc (interleave-all numbers orders)))
                                []
                                (combo/selections ["+" "*"] (dec (count numbers))))])))))
