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


(let [[ordering-rules _ updates] (->> "2024/day05-example.txt"
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
                              (update acc :updates conj page)
                              (do
                                #d [page (set/intersection (set ordering) before-others)]
                                (-> acc
                                    (update :incorrect-pages conj page)
                                    (update :updates conj page)))))) {:updates [] :incorrect-pages []} updates))))
       #_(filter (comp (complement empty?) :incorrect-pages))
       #_(map :updates)
       #_#_(map middle-page)
         (reduce +))
  ordering)

(user/logs 0)


[[75 #{[75 47] [75 53] [75 61] [75 97]}]
 [13 #{[13 29]}]
 [13 #{[13 29] [13 47] [13 75]}]
 [29 #{[29 47]}]
 [75 #{[75 47] [75 53] [75 61] [75 97]}]
 [13 #{[13 29]}]
 [13 #{[13 29] [13 47] [13 75]}]
 [29 #{[29 47]}]]

({:incorrect-pages [], :updates [75 47 61 53 29]}
 {:incorrect-pages [], :updates [97 61 53 29 13]}
 {:incorrect-pages [], :updates [75 29 13]}
 {:incorrect-pages [75], :updates [75 97 47 61 53]}    ;; fixed: 97,75,47,61,53.
 {:incorrect-pages [13], :updates [61 13 29]}          ;; fixed: 61,29,13
 {:incorrect-pages [13 29], :updates [97 13 75 29 47]});; fixed: 97,75,47,29,13
