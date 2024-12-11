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

(defn evaluate [expressions]
  (first (reduce
          (fn [[total operand] x]
            (cond
              (and (nil? total) (nil? operand) (int? x))
              [x nil]

              (or (= x "*") (= x "+"))
              [total x]

              (and (int? x) (= operand "*"))
              [(* total x) nil]

              (and (int? x) (= operand "+"))
              [(+ total x) nil]

              :else
              [total operand]))
          [nil nil]
          expressions)))

(->> "2024/day07.txt"
     io/resource
     io/reader
     line-seq
     (map (partial re-seq #"\d+"))
     (map (partial map parse-long))
     (map (partial (fn [nums]
                     (let [test-value (first nums)
                           numbers    (rest nums)]
                       [test-value
                        (reduce
                         (fn [acc pattern]
                           (if (= test-value (evaluate (interleave-all numbers pattern)))
                             (reduced true)
                             acc))
                         false
                         (combo/selections ["+" "*"] (dec (count numbers))))]))))
     (filter (comp true? second))
     (map first)
     (reduce +))
;; => 2314935962622

(defn evaluate2 [expressions]
  (first (reduce
          (fn [[total operand] x]
            (cond
              (and (nil? total) (nil? operand) (int? x))
              [x nil]

              (or (= x "*") (= x "+") (= x "||"))
              [total x]

              (and (int? x) (= operand "*"))
              [(* total x) nil]

              (and (int? x) (= operand "+"))
              [(+ total x) nil]

              (and (int? x) (= operand "||"))
              [(parse-long (str total x)) nil]

              :else
              [total operand]))
          [nil nil]
          expressions)))

(->> "2024/day07.txt"
     io/resource
     io/reader
     line-seq
     (map (partial re-seq #"\d+"))
     (map (partial map parse-long))
     (map (partial (fn [nums]
                     (let [test-value (first nums)
                           numbers    (rest nums)]
                       [test-value
                        (reduce
                         (fn [acc pattern]
                           (if (= test-value (evaluate2 (interleave-all numbers pattern)))
                             (reduced true)
                             acc))
                         false
                         (combo/selections ["+" "*" "||"] (dec (count numbers))))]))))
     (filter (comp true? second))
     (map first)
     (reduce +))
;; => 401477450831495
