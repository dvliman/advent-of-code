(ns dvliman.2024.day06
  (:require [clojure.java.io :as io]))

(def up    [-1 0])
(def down  [1 0])
(def right [0 1])
(def left  [0 -1])
(def next-direction {up right
                     right down
                     down left
                     left up})

(defn walk [[dx dy] [row col] obstacles]
  (->> [row col]
       (iterate (fn [[row col]]
                  [(+ row dx) (+ col dy)]))
       (take-while (fn [[row col]]
                     (not (contains? (set obstacles) [row col]))))))

(->> "2024/day06-example.txt"
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
            (let [pathways (walk up starting-location obstacle-locations)]
              (if (seq pathways)
                (do
                  (prn "last-pathways: " (last pathways) " pathways: " pathways)
                  (recur (concat result pathways) (last pathways) (get next-direction direction)))
                (concat result pathways))))))))

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

(walk up [6 4] #{[7 8] [9 6] [1 9] [4 7] [8 0] [6 1] [0 4] [3 2]})
;; => ([6 4] [5 4] [4 4] [3 4] [2 4] [1 4])

(walk right [1 4] #{[7 8] [9 6] [1 9] [4 7] [8 0] [6 1] [0 4] [3 2]})
;; => ([1 4] [1 5] [1 6] [1 7] [1 8])

(walk right [1 8] #{[7 8] [9 6] [1 9] [4 7] [8 0] [6 1] [0 4] [3 2]})
;; => [1 0]
