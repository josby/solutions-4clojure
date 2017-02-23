(ns solutions-4clojure.easy-problems)

; 19 - Last Element
; https://www.4clojure.com/problem/19
;
; Write a function which returns the last element in a sequence.
; Special Restrictions: last
;(= (__ [1 2 3 4 5]) 5)
;(= (__ '(5 4 3)) 3)
;(= (__ ["b" "c" "d"]) "d")
(defn last-element [xs]
  ((comp first reverse) xs))

; 20 - Penultimate Element
; https://www.4clojure.com/problem/20
;
; Write a function which returns the second to last element from a sequence.
;(= (__ (list 1 2 3 4 5)) 4)
;(= (__ ["a" "b" "c"]) "b")
;(= (__ [[1 2] [3 4]]) [1 2])
(defn penultimate-element [xs]
  ((comp second reverse) xs))