(ns dvliman.2024.day06
  (:require [clojure.java.io :as io]))

(def up    [-1 0])
(def down  [1 0])
(def right [0 1])
(def left  [0 -1])

(def next-direction
  {up right
   right down
   down left
   left up})

(defn walk [[dx dy] [row col] obstacles grid]
  (->> [row col]
       (iterate (fn [[row col]]
                  [(+ row dx) (+ col dy)]))
       (take-while (fn [[row col]]
                     (or (not (contains? (set obstacles) [row col]))
                         (>= row (count grid))
                         (>= col (count (first grid))))))))

#_(->> "2024/day06-example.txt"
     io/resource
     io/reader
     line-seq
     (map vec)
     vec
     ((fn [grid]
        (->> (for [row (range (count grid))
                   col (range (count (first grid)))]
               (cond
                 (= \^ (get-in grid [row col]))
                 [:guard [row col]]

                 (= \# (get-in grid [row col]))
                 [:obstacle [row col]]

                 :else
                 nil)))))
     (filter some?)
     ((fn [locations]
        (let [{:keys [guard obstacle]} (group-by first locations)
              guard-location     (-> guard last last)
              obstacle-locations (->> obstacle (map second))]
          (loop [result [] starting-location guard-location direction up]
            (prn  "starting: " starting-location ", direction: " direction)
            (let [pathways (walk direction starting-location obstacle-locations)]
              (prn " pathways: " pathways)
              (if (seq pathways)
                (recur (concat result pathways) (last pathways) (get next-direction direction))
                (do
                  (prn "hitting end")
                  result))))))))


(def grid [[\. \. \. \. \# \. \. \. \. \.]
           [\. \. \. \. \. \. \. \. \. \#]
           [\. \. \. \. \. \. \. \. \. \.]
           [\. \. \# \. \. \. \. \. \. \.]
           [\. \. \. \. \. \. \. \# \. \.]
           [\. \. \. \. \. \. \. \. \. \.]
           [\. \# \. \. \^ \. \. \. \. \.]
           [\. \. \. \. \. \. \. \. \# \.]
           [\# \. \. \. \. \. \. \. \. \.]
           [\. \. \. \. \. \. \# \. \. \.]])


#_(walk down [7 7]  '([7 8] [9 6] [1 9] [4 7] [8 0] [6 1] [0 4] [3 2]) grid)
