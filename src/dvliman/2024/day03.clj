(ns dvliman.2024.day03
  (:require [clojure.java.io :as io]))

(defn parse-int [x]
  (Integer/parseInt x))

(->> "2024/day03.txt"
     io/resource
     io/reader
     line-seq
     (apply str)
     (re-seq #"mul\(([\d]+),([\d]+)\)")
     (map (juxt (comp parse-int second) (comp parse-int last)))
     (map (partial apply *))
     (reduce +))
;; => 166357705

(->> "2024/day03.txt"
     io/resource
     io/reader
     line-seq
     (apply str)
     (re-seq #"(mul\(([\d]+),([\d]+)\)|do\(\)|don\'t\(\))")
     (reduce (fn [[enabled total] [_ operand left right]]

               (cond
                 (and enabled (some? left) (some? right))
                 [enabled (+ total (* (parse-int left) (parse-int right)))]

                 (= operand "do()")
                 [true total]

                 (= operand "don't()")
                 [false total]

                 :else
                 [enabled total])
               ) [true 0])
     last)
;; => 88811886
