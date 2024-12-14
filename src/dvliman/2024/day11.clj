(ns dvliman.2024.day11
  (:require [clojure.java.io :as io]))

(->> "2024/day11.txt"
     io/resource
     io/reader
     slurp
     (re-seq #"\d+")
     (map parse-long)
     vec
     (iterate (fn [stones]
                (reduce (fn [acc x]
                          (cond
                            (zero? x)
                            (conj acc 1)

                            (even? (count (str x)))
                            (let [mid (/ (count (str x)) 2)
                                  len (count (str x))
                                  first-half  (parse-long (apply str (subvec (vec (str x)) 0 mid)))
                                  second-half (parse-long (apply str (subvec (vec (str x)) mid len)))]
                              (conj acc first-half second-half))

                            :else
                            (conj acc (* x 2024)))) [] stones)))
     (take (inc 75))
     last
     count)
;; => 231278 (part 1 blink 25 times)
