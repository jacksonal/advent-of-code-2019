(ns advent_of_code_2019.day2
  (:require [clojure.math.combinatorics :as combo]))

(def out-reg (atom nil))

(defn get-op-code
  [instruction]
  (mod instruction 100))

(defn get-op-size
  [op-code]
  (case op-code
    (1 2 7 8) 4
    (5 6) 3
    (3 4 9) 2))

(defn get-param-mode
  [instruction pos]
  (mod (quot instruction (int (Math/pow 10 (+ 1 pos)))) 10))

(defn get-param-value
  [full-input op-seq pos rel-offset]
  (let [param (nth op-seq pos)]
    (case (get-param-mode (nth op-seq 0) pos)
      2 (nth full-input (+ param rel-offset))
      1 param
      0 (nth full-input param))))

(defn add
  [full-input op-seq instr-ptr input-stream rel-offset]
  ;(println full-input op-seq rel-offset)
  [(into
    (into (subvec full-input 0 (nth op-seq 3))
          (vector (+ (get-param-value full-input op-seq 1 rel-offset) (get-param-value full-input op-seq 2 rel-offset))))
    (subvec full-input (+ (nth op-seq 3) 1)))
   (+ instr-ptr 4)
   input-stream
   rel-offset])

(defn mult
  [full-input op-seq instr-ptr input-stream rel-offset]
  [(into
     (into (subvec full-input 0 (nth op-seq 3))
           (vector (* (get-param-value full-input op-seq 1 rel-offset) (get-param-value full-input op-seq 2 rel-offset))))
     (subvec full-input (+ (nth op-seq 3) 1)))
   (+ instr-ptr 4)
   input-stream
   rel-offset])

(defn write
  [full-input op-seq instr-ptr input-stream rel-offset]
  ;(print instr-ptr op-seq (take 38 full-input))
  (println "INPUT REQUIRED")
  [(into
    (into (subvec full-input 0 (nth op-seq 1))
          (vector  
           (if (= (count input-stream) 0) ; take stream input if we can
             (Integer/parseInt (read-line))
             (first input-stream))))
    (subvec full-input (+ (nth op-seq 1) 1)))
   (+ instr-ptr 2)
   (drop 1 input-stream)
   rel-offset]
  )

(defn output
  [full-input op-seq instr-ptr input-stream rel-offset]
  (let [out (get-param-value full-input op-seq 1 rel-offset)]
    (println "OUTPUT:" out)
    (reset! out-reg out))
  [full-input 
   (+ instr-ptr 2)
   input-stream
   rel-offset])

(defn lt
  [full-input op-seq instr-ptr input-stream rel-offset]
  [(into
     (into (subvec full-input 0 (nth op-seq 3))
           (vector (if (< (get-param-value full-input op-seq 1 rel-offset) (get-param-value full-input op-seq 2 rel-offset))
                     1
                     0)))
     (subvec full-input (+ (nth op-seq 3) 1)))
   (+ instr-ptr 4)
   input-stream
   rel-offset])

(defn eq
  [full-input op-seq instr-ptr input-stream rel-offset]
  [(into
     (into (subvec full-input 0 (nth op-seq 3))
           (vector (if (= (get-param-value full-input op-seq 1 rel-offset) (get-param-value full-input op-seq 2 rel-offset))
                     1
                     0)))
     (subvec full-input (+ (nth op-seq 3) 1)))
   (+ instr-ptr 4)
   input-stream
   rel-offset])

(defn jump-true
  [full-input op-seq instr-ptr input-stream rel-offset]
  [full-input 
   (if (not= (get-param-value full-input op-seq 1 rel-offset) 0)
     (get-param-value full-input op-seq 2 rel-offset)
     (+ instr-ptr 3))
   input-stream
   rel-offset])

(defn jump-false
  [full-input op-seq instr-ptr input-stream rel-offset]
  [full-input 
   (if (= (get-param-value full-input op-seq 1 rel-offset) 0)
     (get-param-value full-input op-seq 2 rel-offset)
     (+ instr-ptr 3))
   input-stream
   rel-offset])

(defn inc-offset
  [full-input op-seq instr-ptr input-stream rel-offset]
  ;(println op-seq rel-offset (+ rel-offset (get-param-value full-input op-seq 1 rel-offset)))
  [full-input
   (+ instr-ptr 2)
   input-stream
   (+ rel-offset (get-param-value full-input op-seq 1 rel-offset))])

(defn operate
  ([full-input op-seq instruction-ptr]
   (operate full-input op-seq instruction-ptr '() 0))
  ([full-input op-seq instruction-ptr input-stream rel-offset]
  (let [op-code (mod (get-op-code (nth op-seq 0)) 10)]
    ;(println op-seq instruction-ptr rel-offset op-code)
    (case op-code
      1 (add full-input op-seq instruction-ptr input-stream rel-offset)
      2 (mult full-input op-seq instruction-ptr input-stream rel-offset)
      3 (write full-input op-seq instruction-ptr input-stream rel-offset)
      4 (output full-input op-seq instruction-ptr input-stream rel-offset)
      5 (jump-true full-input op-seq instruction-ptr input-stream rel-offset)
      6 (jump-false full-input op-seq instruction-ptr input-stream rel-offset)
      7 (lt full-input op-seq instruction-ptr input-stream rel-offset) 
      8 (eq full-input op-seq instruction-ptr input-stream rel-offset)
      9 (inc-offset full-input op-seq instruction-ptr input-stream rel-offset)))))

(defn recur-int-compute
  ([full-input start]
   (recur-int-compute full-input start '() 0))
  ([full-input start input-stream]
   (recur-int-compute full-input start input-stream 0))
  ([full-input start input-stream rel-offset]
   ;(println start)
   (let [instruction (get-op-code (nth full-input start))]
     (if (= 99 instruction)
       @out-reg
       (let [op-size (get-op-size (mod instruction 10))
             op-result (operate full-input
                                (subvec full-input start (+ start op-size))
                                start
                                input-stream
                                rel-offset)]
         ;(println (drop 1 op-result))
         (recur (nth op-result 0)
                (nth op-result 1)
                (nth op-result 2)
                (nth op-result 3)))))))

(defn run-diagnostic
  "day 5"
  []
  (let [input (into (vector) (map #(Integer/parseInt %) (clojure.string/split
                                                         (slurp "resources/input_day5.txt")
                                                         #",")))]
    (recur-int-compute input 0 [1])))

(defn run-amp-seq
  [input phase-seq]
  (let [amp-seq (map (fn 
                       [phase-setting] 
                       #(recur-int-compute input 0 [phase-setting %]))
                    phase-seq)]
    (reduce (fn [signal amp] (amp signal)) 0 amp-seq)))

(defn optimize-amp
  "day 7"
  []
  (let [input (into (vector) (map #(Integer/parseInt %) (clojure.string/split
                                                         (slurp "resources/input_day7.txt")
                                                         #",")))
        phase-perm (combo/permutations [0 1 2 3 4])]
    (apply max (map (fn [phase-seq] (run-amp-seq input phase-seq)) phase-perm))))

(defn run-boost-diagnostic
  "day 9"
  []
  (let [input (into (vector) (map #(Integer/parseInt %) (clojure.string/split
                                                         (slurp "resources/input_day7.txt")
                                                         #",")))]
    (print (map vector (range 40) (take 40 input)))
    (recur-int-compute (vec (concat input (repeat 5000 0))) 0)))

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