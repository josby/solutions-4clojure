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
