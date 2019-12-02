(defn get-fuel-req
  [mass]
  (- (quot mass 3) 2))

(defn get-fuel-req-recursive
  [fuel-mass]
  (if (<= fuel-mass 0)
    0
    (+ fuel-mass (get-fuel-req-recursive (get-fuel-req fuel-mass)))))

(defn how-much-fuel
  []
  (def input (map #(Integer/parseInt %) (clojure.string/split
                                         (slurp "resources/input_day1.txt")
                                         #"\s+")))
  (reduce + (map get-fuel-req-recursive (map get-fuel-req input))))