(ns dvliman.2024.day05
  (:require [clojure.java.io :as io]))


(let [[ordering _ updates] (->> "2024/day5-example.txt"
                                io/resource
                                io/reader
                                line-seq
                                (partition-by #(= "" %)))
      ordering-rules (->> ordering
                          (map (partial re-seq #"\d+"))
                          (map (juxt (comp parse-long first) (comp parse-long last))))]
  #_(->> updates
       (map (partial re-seq #"\d+"))
       (map (partial map parse-long))
       (map (partial map (fn [update]
                           update)))))

#_21
#_([47 53]
 [97 13]
 [97 61]
 [97 47]
 [75 29]
 [61 13]
 [75 53]
 [29 13]
 [97 29]
 [53 29]
 [61 53]
 [97 53]
 [61 29]
 [47 13]
 [75 47]
 [97 75]
 [47 61]
 [75 61]
 [47 29]
 [75 13]
 [53 13])
