(defn count-digits
  "reduce to a tuple of number of 0s, 1s, and 2s"
  [layer]
  [(reduce (fn 
            [seed pixel]
            (if (zero? pixel)
              (inc seed)
              seed)) 0 layer)
  (reduce (fn
            [seed pixel]
            (if (= pixel 1)
              (inc seed)
              seed)) 0 layer)
  (reduce (fn
            [seed pixel]
            (if (= pixel 2)
              (inc seed)
              seed)) 0 layer)])

(defn check-image-integrity
  []
  (let [input (into (vector) (map #(Integer/parseInt %) (clojure.string/split
                                                         (slurp "resources/input_day8.txt")
                                                         #"")))
        layers (partition 6 (partition 25 input))
        min-zero-layer (apply min-key #(first %) (map count-digits (map flatten layers)))]
    (* (nth min-zero-layer 1) (nth min-zero-layer 2))))