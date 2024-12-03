(ns dvliman.2024.day02
  (:require [clojure.java.io :as io]))

(defn tolerance [x y]
  (<= 1 (Math/abs (- x y)) 3))

(defn mode [x y]
  (cond
    (< x y) :increasing
    (> x y) :decreasing
    :else :neither))

#_((7 6 4 2 1) (1 2 7 8 9) (9 7 6 2 1) (1 3 2 4 5) (8 6 4 4 1) (1 3 6 7 9))

(defn analyze [{:keys [last-level] :as acc} level]
  (if last-level
    (-> acc
        (assoc :last-level level)
        (update :modes conj (mode last-level level))
        (update :tolerances conj (tolerance last-level level)))
    (assoc acc :last-level level)))

(defn safe? [{:keys [modes tolerances]}]
  (and (== 1 (count (set modes)))
       (not= :neither (first modes))
       (== 1 (count (set tolerances)))
       (true? (first tolerances))))

(->> "2024/day2.txt"
     io/resource
     io/reader
     line-seq
     (map (partial re-seq #"\d+"))
     (map (partial map #(Integer/parseInt %)))
     (map (partial reduce analyze {:modes [] :tolerances [] :last-level nil}))
     (filter safe?)
     count)
;; => 598

(defn ignore-one [xs target-index]
  (vec (keep-indexed (fn [i x]
                       (when (not= i target-index) x)) xs)))

(defn permute [levels]
  (map-indexed (fn [i _]
                 (ignore-one levels i)) levels))

#_(permute '(8 6 4 4 1))
;; => ([6 4 4 1] [8 4 4 1] [8 6 4 1] [8 6 4 1] [8 6 4 4])

(let [{safes true unsafes false}
      (->> "2024/day2.txt"
           io/resource
           io/reader
           line-seq
           (map (partial re-seq #"\d+"))
           (map (partial map #(Integer/parseInt %)))
           (map (fn [levels]
                  (assoc
                   (reduce analyze {:modes [] :tolerances [] :last-level nil} levels)
                   :original levels)))
           (group-by safe?))]
  (+ (count safes)
     ;; attempt to tolerates single bad level
     (->> unsafes
          (map :original)
          (map permute)
          (map (partial map (partial (fn [levels]
                                       (assoc
                                        (reduce analyze {:modes [] :tolerances [] :last-level nil} levels)
                                        :original levels)))))
          (map (partial filter safe?))
          (filter seq)
          count)))
;; => 634

;; original
#_'((1 2 7 8 9) (9 7 6 2 1) (1 3 2 4 5) (8 6 4 4 1))
;; permuted
#_'(([2 7 8 9] [1 7 8 9] [1 2 8 9] [1 2 7 9] [1 2 7 8])
 ([7 6 2 1] [9 6 2 1] [9 7 2 1] [9 7 6 1] [9 7 6 2])
 ([3 2 4 5] [1 2 4 5] [1 3 4 5] [1 3 2 5] [1 3 2 4])
 ([6 4 4 1] [8 4 4 1] [8 6 4 1] [8 6 4 1] [8 6 4 4]))
