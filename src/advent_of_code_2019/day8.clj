(defn count-zeros
  [layer]
  (reduce (fn 
            [seed pixel]
            (if (zero? pixel)
              (inc seed)
              seed)) 0 layer))

(defn check-image-integrity
  []
  (let [input (into (vector) (map #(Integer/parseInt %) (clojure.string/split
                                                         (slurp "resources/input_day8.txt")
                                                         #"")))
        layers (partition 6 (partition 25 input))]
    (apply min (map count-zeros (map flatten layers)))))