(defn operate
  [full-input op-seq]
  (into
   (into (subvec full-input 0 (nth op-seq 3))
         (if (== 1 (nth op-seq 0))
           (vector (+ (nth full-input (nth op-seq 1)) (nth full-input (nth op-seq 2))))
           (if (== 2 (nth op-seq 0))
             (vector (* (nth full-input (nth op-seq 1)) (nth full-input (nth op-seq 2)))))))
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
  (nth (filter
        (fn [x] (not (nil? x)))
        (flatten
         (for [noun (range 100)]
           (for [verb (range 100)]
             (let [result (nth (recur-int-compute (into (into (subvec input 0 1) [noun verb]) (subvec input 3)) 0) 0)]
               (if (== result 19690720)
                 (+ (* 100 noun) verb))))))) 
       0))