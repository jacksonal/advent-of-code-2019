(ns advent-of-code-2019.core
  (:gen-class))

(defn get-fuel-req
  [mass]
  (- (quot mass 3) 2))

(defn get-fuel-req-recursive
  [fuel-mass]
  (if (<= fuel-mass 0)
    0
    (+ fuel-mass (get-fuel-req-recursive (get-fuel-req fuel-mass)))
  )
)

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  
  (def input (map #(Integer/parseInt %) (clojure.string/split
              (slurp "resources/input_day1.txt")
              #"\s+")))
  (println (reduce + (map get-fuel-req-recursive (map get-fuel-req input))))
  )