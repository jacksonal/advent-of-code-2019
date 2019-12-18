(defrecord Body [position velocity])

(def sample-bodies
  [(Body. [-1 0 2] [0 0 0])
   (Body. [2 -10 -7] [0 0 0])
   (Body. [4 -8 8] [0 0 0])
   (Body. [3 5 -1] [0 0 0])])

(defn apply-gravity
  [[planetA planetB]]
  (map (fn
         [[a-pos b-pos]]
         (if (< a-pos b-pos)
           [1 -1]
           (if (> a-pos b-pos)
             [-1 1]
             [0 0]))) (map vector planetA planetB)))

(defn apply-velocity
  [planet]
  (let [new-pos (map (fn [[pos vel]] (+ pos vel)) 
                     (map vector (planet :position) (planet :velocity)))]
    (Body. new-pos (planet :velocity))))