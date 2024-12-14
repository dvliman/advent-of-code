(ns dvliman.2024.day08
  (:require [clojure.java.io :as io]))

(->> "2024/day08-example.txt"
     io/resource
     io/reader
     line-seq
     (map vec)
     vec)
