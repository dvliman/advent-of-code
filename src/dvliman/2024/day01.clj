(ns dvliman.2024.day01
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn read-lines [file-path]
  (with-open [reader (io/reader (io/resource file-path))]
    (doall (line-seq reader))))

(defn parse-int [x]
  (Integer/parseInt x))

(let [locations (->> "2024/day01.txt"
                     read-lines
                     (map #(str/split % #" ")))
      group1 (sort (map (comp parse-int first) locations))
      group2 (sort (map (comp parse-int last) locations))]
  (->>
   (map vector group1 group2)
   (map (fn distance' [[x y]] (Math/abs (- y x))))
   (reduce +)))
;; => 2031679

(let [locations (->> "2024/day01.txt"
                     read-lines
                     (map #(str/split % #" ")))
      group1 (sort (map (comp parse-int first) locations))
      freqs (frequencies (map (comp parse-int last) locations))]
  (->>
   group1
   (map (fn similarity' [x] (* x (get freqs x 0))))
   (reduce +)))
;; => 19678534
