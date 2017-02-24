(ns solutions-4clojure.easy-problems)

; 19 - Last Element
; https://www.4clojure.com/problem/19
;
; Write a function which returns the last element in a sequence.
; Special Restrictions: last
; (= (__ [1 2 3 4 5]) 5)
; (= (__ '(5 4 3)) 3)
; (= (__ ["b" "c" "d"]) "d")
(defn last-element [xs]
  ((comp first reverse) xs))


; 20 - Penultimate Element
; https://www.4clojure.com/problem/20
;
; Write a function which returns the second to last element from a sequence.
; (= (__ (list 1 2 3 4 5)) 4)
; (= (__ ["a" "b" "c"]) "b")
; (= (__ [[1 2] [3 4]]) [1 2])
(defn penultimate-element [xs]
  ((comp second reverse) xs))


; 21 - Nth Element
; https://www.4clojure.com/problem/21
;
; Write a function which returns the Nth element from a sequence.
; Special Restrictions: nth
; (= (__ '(4 5 6 7) 2) 6)
; (= (__ [:a :b :c] 0) :a)
; (= (__ [1 2 3 4] 1) 2)
; (= (__ '([1 2] [3 4] [5 6]) 2) [5 6])
(defn nth-element [xs position]
  (cond
    (zero? position) (first xs) ; base case - return element at 0 position
    (pos? position) (recur (rest xs) (dec position))
    (neg? position) -1))


; 22 - Count a Sequence
; https://www.4clojure.com/problem/22
;
; Write a function which returns the total number of elements in a sequence.
; Special Restrictions: count
;(= (__ '(1 2 3 3 1)) 5)
;(= (__ "Hello World") 11)
;(= (__ [[1 2] [3 4] [5 6]]) 3)
;(= (__ '(13)) 1)
;(= (__ '(:a :b :c)) 3)
(defn count-a-sequence [xs]
  (->> xs
       (map (constantly 1))
       (reduce +)))


; 23 - Reverse a Sequence
; https://www.4clojure.com/problem/23
;
; Write a function which reverses a sequence.
; Special Restrictions: reverse, rseq
; (= (__ [1 2 3 4 5]) [5 4 3 2 1])
; (= (__ (sorted-set 5 7 2 7)) '(7 5 2))
; (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])
(defn reverse-a-sequence [xs]
  (reduce (fn [acc, current] (conj acc current)) '() xs))


; 24 - Sum It All Up
; https://www.4clojure.com/problem/24
;
; Write a function which returns the sum of a sequence of numbers.
; (= (__ [1 2 3]) 6)
; (= (__ (list 0 -2 5 5)) 8)
; (= (__ #{4 2 1}) 7)
; (= (__ '(0 0 -1)) -1)
; (= (__ '(1 10 3)) 14)
(defn sum [xs]
  (reduce + xs))


; 25 - Find the odd numbers
; https://www.4clojure.com/problem/25
;
; Write a function which returns only the odd numbers from a sequence.
; (= (__ #{1 2 3 4 5}) '(1 3 5))
; (= (__ [4 2 1 6]) '(1))
; (= (__ [2 2 4 6]) '())
; (= (__ [1 1 1 3]) '(1 1 1 3))
(defn find-the-odd-numbers [xs]
  (filter odd? xs))


; 26 - Fibonacci Sequence
; https://www.4clojure.com/problem/26
;
; Write a function which returns the first X fibonacci numbers.
; (= (__ 3) '(1 1 2))
; (= (__ 6) '(1 1 2 3 5 8))
; (= (__ 8) '(1 1 2 3 5 8 13 21))
(defn fib [n]
  (letfn [(generate-fib-seq
            ([] (generate-fib-seq 1 1))
            ([n m] (cons n (lazy-seq (generate-fib-seq m (+ n m))))))]
    (take n (generate-fib-seq))))


; 27 - Palindrome Detector
; https://www.4clojure.com/problem/27
;
; Write a function which returns true if the given sequence is a palindrome.
; Hint: "racecar" does not equal '(\r \a \c \e \c \a \r)
; (false? (__ '(1 2 3 4 5)))
; (true? (__ "racecar"))
; (true? (__ [:foo :bar :foo]))
; (true? (__ '(1 1 3 3 1 1)))
; (false? (__ '(:a :b :c)))
(defn palindrome? [xs]
  (if (empty? xs)
    true ;base case
    (let [first-element (first xs)
          last-element (last xs)
          list-without-first-and-last (drop-last (rest xs))]

      (and (= first-element last-element)
           (recur list-without-first-and-last)))))


; 28 - Flatten a Sequence
; https://www.4clojure.com/problem/28
;
; Write a function which flattens a sequence.
; Special Restrictions flatten
; (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
; (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
; (= (__ '((((:a))))) '(:a))
(defn flatten-seq [xs]
      (letfn [(flattify [acc, x]
            (if (sequential? x)
              (reduce flattify acc x)
              (conj acc x)))]

    (reduce flattify [] xs)))