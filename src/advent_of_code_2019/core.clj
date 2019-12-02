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

(defn operate
  [full-input op-seq]
  (into
   (into (subvec full-input 0 (nth op-seq 3))
         (if (== 1 (nth op-seq 0))
           (vector (+ (nth full-input (nth op-seq 1)) (nth full-input (nth op-seq 2))))
           (if (== 2 (nth op-seq 0))
             (vector (* (nth full-input (nth op-seq 1)) (nth full-input (nth op-seq 2))))
             (if (== 99 (nth op-seq 0))
               (comment "end")
               (comment "problem")))))
   (subvec full-input (+ (nth op-seq 3) 1))))

(defn recur-int-compute
  [full-input start]
  (if (== 99 (nth full-input start))
    full-input
    (recur (operate full-input (subvec full-input start (+ start 4))) (+ start 4))))

(defn int-compute
  []
  (def input (into (vector) (map #(Integer/parseInt %) (clojure.string/split
                                                 (slurp "resources/input_day2.txt")
                                                 #","))))
  (recur-int-compute (into (into (subvec input 0 1) [12 2]) (subvec input 3)) 0)
  )

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  
  (def input (map #(Integer/parseInt %) (clojure.string/split
              (slurp "resources/input_day1.txt")
              #"\s+")))
  (println "day 1:" (reduce + (map get-fuel-req-recursive (map get-fuel-req input))))
  )