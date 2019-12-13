(defn solve1
  "Create a hierarchy to track orbits and map reduce the distances between all planets"
  []
  (let [input (map #(mapv keyword %) (map #(clojure.string/split % #"\)")
                                          (clojure.string/split-lines (slurp "resources/input_day6.txt"))))
        universe (reduce (fn [h [parent child]]
                           (derive h child parent))
                         (make-hierarchy)
                         input)]
    (reduce + (map #(count (ancestors universe %)) (descendants universe :COM)))))

(defn solve2
  "determine number of jumps to get from me to santa"
  []
  (let [input (map #(mapv keyword %) (map #(clojure.string/split % #"\)")
                                          (clojure.string/split-lines (slurp "resources/input_day6.txt"))))
        universe (reduce (fn [h [parent child]]
                           (derive h child parent))
                         (make-hierarchy)
                         input)
        my-orbits (ancestors universe :YOU)
        santas-orbits (ancestors universe :SAN)]
    (+ (count (clojure.set/difference my-orbits santas-orbits))
       (count (clojure.set/difference santas-orbits my-orbits)))))