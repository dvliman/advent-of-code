(ns dvliman.2024.day06
  (:require [clojure.java.io :as io]
            [clojure.set :as set]))

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

(->> "2024/day06-example.txt"
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
              ;; TODO: terminating too early on [4 2] going right, seen [4 4]
              (when (= [4 2] starting-location)
                #_#_(prn (set result))
                (prn (set pathways))
                (prn (set/intersection (set result) (set pathways))))
              (if (empty? (set/intersection (set result) (set pathways)))
                (recur (concat result pathways) (last pathways) (get next-direction direction))
                result)))))))


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
(set/intersection #{[1 2] [2 3]} #{[3 4]})
(walk down [7 7] '([0 4] [1 9] [3 2] [4 7] [6 1] [7 8] [8 0] [9 6]) grid)
;; => ([7 7] [8 7] [9 7]) this is the last step - it shouldn't return [9 7]

(walk down [9 7] '([0 4] [1 9] [3 2] [4 7] [6 1] [7 8] [8 0] [9 6]) grid)
;; => ([9 7]) return itself

(walk right [5 7] '([0 4] [1 9] [3 2] [4 7] [6 1] [7 8] [8 0] [9 6]) grid)
;; => ([5 7] [5 8] [5 9])


(walk down [5 9] '([0 4] [1 9] [3 2] [4 7] [6 1] [7 8] [8 0] [9 6]) grid)
;; => ([5 9] [6 9] [7 9] [8 9] [9 9])

(walk left [9 9] '([0 4] [1 9] [3 2] [4 7] [6 1] [7 8] [8 0] [9 6]) grid)
;; => ([9 9] [9 8] [9 7])
