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

(defn apply-mask
  [top bottom]
  (let [flat-top (flatten top)
        flat-bottom (flatten bottom)]
    (map (fn [[top-pixel bottom-pixel]]
           (if (= top-pixel 2)
             bottom-pixel
             top-pixel)) (map vector flat-top flat-bottom))))
  
(defn combine-layers
  [layers]
  (reduce apply-mask layers))
(defn check-image-integrity
  "find the layer with the fewest 0s and multiply the number of 1s by the number of 2s"
  []
  (let [input (into (vector) (map #(Integer/parseInt %) (clojure.string/split
                                                         (slurp "resources/input_day8.txt")
                                                         #"")))
        layers (partition 6 (partition 25 input))
        min-zero-layer (apply min-key #(first %) (map count-digits (map flatten layers)))]
    (* (nth min-zero-layer 1) (nth min-zero-layer 2))))

(defn view-image
  "0 - black 1 - white 2 - transparent"
  []
  (let [input (into (vector) (map #(Integer/parseInt %) (clojure.string/split
                                                         (slurp "resources/input_day8.txt")
                                                         #"")))
        layers (partition 6 (partition 25 input))]
    (combine-layers layers)))