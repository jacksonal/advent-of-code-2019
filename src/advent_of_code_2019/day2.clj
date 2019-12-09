
(defn get-op-code
  [instruction]
  (mod instruction 100))

(defn get-op-size
  [op-code]
  (case op-code
    (1 2 7 8) 4
    (5 6) 3
    (3 4) 2))

(defn get-param-mode
  [instruction pos]
  (mod (quot instruction (int (Math/pow 10 (+ 1 pos)))) 10))

(defn get-param-value
  [full-input op-seq pos]
  (let [param (nth op-seq pos)]
    (case (get-param-mode (nth op-seq 0) pos)
      1 param
      0 (nth full-input param))))

(defn add 
  [full-input op-seq instr-ptr]
  [(into
    (into (subvec full-input 0 (nth op-seq 3))
          (vector (+ (get-param-value full-input op-seq 1) (get-param-value full-input op-seq 2))))
    (subvec full-input (+ (nth op-seq 3) 1))) 
   (+ instr-ptr 4)])

(defn mult
  [full-input op-seq instr-ptr]
  [(into
    (into (subvec full-input 0 (nth op-seq 3))
          (vector (* (get-param-value full-input op-seq 1) (get-param-value full-input op-seq 2))))
    (subvec full-input (+ (nth op-seq 3) 1)))
   (+ instr-ptr 4)])

(defn write
  [full-input op-seq instr-ptr]
  (println "INPUT REQUIRED")
  [(into
    (into (subvec full-input 0 (nth op-seq 1))
          (vector (Integer/parseInt (read-line))))
    (subvec full-input (+ (nth op-seq 1) 1)))
   (+ instr-ptr 2)])

(defn output
  [full-input op-seq instr-ptr]
  (println "OUTPUT:" (get-param-value full-input op-seq 1))
  [full-input (+ instr-ptr 2)])

(defn lt
  [full-input op-seq instr-ptr]
  [(into
    (into (subvec full-input 0 (nth op-seq 3))
          (vector (if (< (get-param-value full-input op-seq 1) (get-param-value full-input op-seq 2))
                    1
                    0)))
    (subvec full-input (+ (nth op-seq 3) 1)))
   (+ instr-ptr 4)])

(defn eq
  [full-input op-seq instr-ptr]
  [(into
    (into (subvec full-input 0 (nth op-seq 3))
          (vector (if (= (get-param-value full-input op-seq 1) (get-param-value full-input op-seq 2))
                    1
                    0)))
    (subvec full-input (+ (nth op-seq 3) 1)))
   (+ instr-ptr 4)])

(defn jump-true
  [full-input op-seq instr-ptr]
  [full-input (if (not= (get-param-value full-input op-seq 1) 0)
                (get-param-value full-input op-seq 2)
                (+ instr-ptr 3))])

(defn jump-false
  [full-input op-seq instr-ptr]
  [full-input (if (= (get-param-value full-input op-seq 1) 0)
                (get-param-value full-input op-seq 2)
                (+ instr-ptr 3))])

(defn operate
  [full-input op-seq instruction-ptr]
  (let [op-code (mod (get-op-code (nth op-seq 0)) 10)] 
    (case op-code
      1 (add full-input op-seq instruction-ptr)
      2 (mult full-input op-seq instruction-ptr)
      3 (write full-input op-seq instruction-ptr)
      4 (output full-input op-seq instruction-ptr)
      5 (jump-true full-input op-seq instruction-ptr)
      6 (jump-false full-input op-seq instruction-ptr)
      7 (lt full-input op-seq instruction-ptr)
      8 (eq full-input op-seq instruction-ptr))))

(defn recur-int-compute
  [full-input start]
  ;(println "program" full-input)
  (let [instruction (get-op-code (nth full-input start))]
    (if (= 99 instruction)
      full-input
      (let [op-size (get-op-size (mod instruction 10))
            op-result (operate full-input
                               (subvec full-input start (+ start op-size))
                               start)]
        (recur (nth op-result 0)
               (nth op-result 1))))))

(defn run-diagnostic
  "day 5"
  []
  (let [input (into (vector) (map #(Integer/parseInt %) (clojure.string/split
                                                         (slurp "resources/input_day5.txt")
                                                         #",")))]
;(operate input [3 225])))
    (recur-int-compute input 0)
    (println "Done")))

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