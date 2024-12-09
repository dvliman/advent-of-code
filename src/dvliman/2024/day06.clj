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
                     (and (not (contains? (set obstacles) [row col]))
                          (< row (count grid))
                          (< col (count (first grid))))))))

(->> "2024/day06.txt"
     io/resource
     io/reader
     line-seq
     (map vec)
     vec
     ((fn [grid]
        [grid
         (->> (for [row (range (count grid))
                    col (range (count (first grid)))]
                (cond
                  (= \^ (get-in grid [row col]))
                  [:guard [row col]]

                  (= \# (get-in grid [row col]))
                  [:obstacle [row col]]

                  :else
                  nil)))]))
     ((fn [[grid locations]]
        (let [{:keys [guard obstacle]} (group-by first (filter some? locations))
              guard-location     (-> guard last last)
              obstacle-locations (->> obstacle (map second))]
          (loop [result [] starting-location guard-location direction up]
            (let [pathways (walk direction starting-location obstacle-locations grid)
                  pathways (remove (partial = starting-location) pathways)]
              (if (seq pathways)
                (recur (concat result pathways) (last pathways) (get next-direction direction))
                result))))))
     #_#_set count)

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

#_(walk left [9 9] '([0 4] [1 9] [3 2] [4 7] [6 1] [7 8] [8 0] [9 6]) grid)
;; => ([9 9] [9 8] [9 7])
