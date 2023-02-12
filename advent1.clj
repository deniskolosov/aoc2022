(ns advent1
  (:require [clojure.string :as s]
            [clojure.walk :as w]
            [clojure.set :as set]))

(let [input (map read-string )]
     input)

(defn advent-1
  []
  (let [input (s/split-lines (slurp  "./1/input"))]
    (loop [weights input
           biggest 0
           cur 0]
      (if (empty? weights)
        biggest
        (let [item (first weights)
              r (rest weights)]
          (if (s/blank? item)
            (recur r biggest 0)
            (let [upd (+ cur (read-string item))]
              (if (> upd biggest)
                (recur r upd upd)
                (recur r biggest upd)))))))))
;; => 75622
;; => 11684945


(defn advent-2
  [input]
  (loop [weights input
         biggest-3 #{}
         cur 0]
    (if (empty? weights)
      (let [min-w (apply min biggest-3)]
        (if (> cur min-w)
          (apply + (conj (remove #{min-w} biggest-3) cur))
          (apply + biggest-3)))
      (let [item (first weights)
            r (rest weights)]
        (if (s/blank? item)
          (if (> (count biggest-3) 2)
            (if (> cur (apply min biggest-3))
              (recur r (set (conj (remove #{(apply min biggest-3)} biggest-3) cur)) 0)
              (recur r biggest-3 0))
            (recur r (conj biggest-3 cur) 0))
          (recur r biggest-3 (+ cur (read-string item))))))))

#_(advent-2 ["1" "2" "" "2" "3" "" "6" "4" "8" "4" "" "100" "200"])
#_(advent-2 (s/split-lines (slurp  "./1/input")));; => 213159

(defn advent-3
  [input]
  (let [draws #{"A X" "B Y" "C Z"}
        wins #{"A Y" "B Z" "C X"}
        losses #{"A Z" "B X" "C Y"}]
    (loop [strategy input
           score 0]
      (if (empty? strategy)
        score
        (let [pair (first strategy)
              shape (last pair)
              shape-points (case shape
                             \X 1
                             \Y 2
                             3)]
          (if (contains? draws pair)
            (recur (rest strategy) (+ score 3 shape-points))
            (if (contains? wins pair)
              (recur (rest strategy) (+ score 6 shape-points))
              (recur (rest strategy) (+ score 0 shape-points)))))))))


(s/split-lines "file")
;; x>z z>y b>a
#_(second (s/split-lines (slurp  "./1/input2")))
#_(advent-3 (s/split-lines (slurp  "./1/input2")))
#_(advent-3 ["A Y" "B X" "C Z"]) ;; 15
