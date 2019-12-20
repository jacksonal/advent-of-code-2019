(ns advent_of_code_2019.day16
  (:require [clojure.math.numeric-tower :as math]))

(def input (map #(Integer/parseInt %) (clojure.string/split
                                  (slurp "resources/input_day16.txt")
                                  #"")))
(def pattern '(0 1 0 -1))

(defn get-pattern
  "return lazy seq"
  [idx]
  (drop 1 (cycle (flatten (map #(repeat (inc idx) %) pattern)))))

(defn phase-calc
  [signal]
    (for [sig-idx (range)
          :while (< sig-idx (count signal))
          :let [pairs (map vector signal (get-pattern sig-idx))]]
      (mod (math/abs (reduce #(+ %1 (reduce * %2)) 0 pairs)) 10)))

(defn do-fft
  []
  (let [input (map #(Integer/parseInt %) (clojure.string/split
                                          (slurp "resources/input_day16.txt")
                                          #""))]
    (time (doall (take 8 (reduce 
             (fn [seed phase-num] 
               (phase-calc seed)) input (range 100)))))))