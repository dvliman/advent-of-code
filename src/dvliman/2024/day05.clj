(ns dvliman.2024.day05
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

(defn befores [v]
  (for [i (range (count v))
        j (range (inc i) (count v))]
    [(nth v i) (nth v j)]))

(defn middle-page [coll]
  (let [mid (Math/floor (/ (count coll) 2))]
    (nth coll mid)))

(let [[ordering-rules _ updates] (->> "2024/day05.txt"
                                      io/resource
                                      io/reader
                                      line-seq
                                      (partition-by #(= "" %)))
      ordering (->> ordering-rules
                    (map (partial re-seq #"\d+"))
                    (map (juxt (comp parse-long first) (comp parse-long last))))]

  (->> updates
       (map (partial re-seq #"\d+"))
       (map (partial map parse-long))
       (map (fn [updates]
              (let [befores (befores updates)]
                (reduce (fn [acc page]
                          (let [before-others (set (filter (comp #(= % page)  first) befores))
                                intersected (set/intersection before-others (set ordering))
                                before-others? (>= (count intersected) (count before-others))]
                            (if before-others?
                              (conj acc page)
                              (reduced :invalid-updates)))) [] updates))))
       (filter (complement keyword?))
       (map middle-page)
       (reduce +)))
;; => 4578
