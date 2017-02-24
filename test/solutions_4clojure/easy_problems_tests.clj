(ns solutions-4clojure.easy-problems-tests
    (:require [clojure.test :refer :all]
      [solutions-4clojure.easy-problems :refer :all]))

(deftest _last-element
  (is (= (last-element [1 2 3 4 5]) 5))
  (is (= (last-element '(5 4 3)) 3))
  (is (= (last-element ["b" "c" "d"]) "d")))

(deftest _penultimate-element
  (is (= (penultimate-element (list 1 2 3 4 5)) 4))
  (is (= (penultimate-element ["a" "b" "c"]) "b"))
  (is (= (penultimate-element [[1 2] [3 4]]) [1 2])))

(deftest _nth-element
  (is (= (nth-element '(4 5 6 7) 2) 6))
  (is (= (nth-element [:a :b :c] 0) :a))
  (is (= (nth-element [1 2 3 4] 1) 2))
  (is (= (nth-element '([1 2] [3 4] [5 6]) 2) [5 6])))

(deftest _count-a-sequence
  (is (= (count-a-sequence '(1 2 3 3 1)) 5))
  (is (= (count-a-sequence "Hello World") 11))
  (is (= (count-a-sequence [[1 2] [3 4] [5 6]]) 3))
  (is (= (count-a-sequence '(13)) 1))
  (is (= (count-a-sequence '(:a :b :c)) 3)))

(deftest _reverse-seq
  (is (= (reverse-seq [1 2 3 4 5]) [5 4 3 2 1]))
  (is (= (reverse-seq (sorted-set 5 7 2 7)) '(7 5 2)))
  (is (= (reverse-seq [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])))

(deftest _sum
  (is (= (sum [1 2 3]) 6))
  (is (= (sum (list 0 -2 5 5)) 8))
  (is (= (sum #{4 2 1}) 7))
  (is (= (sum '(0 0 -1)) -1))
  (is (= (sum '(1 10 3)) 14)))

(deftest _find-the-odd-numbers
  (is (= (find-the-odd-numbers #{1 2 3 4 5}) '(1 3 5)))
  (is (= (find-the-odd-numbers [4 2 1 6]) '(1)))
  (is (= (find-the-odd-numbers [2 2 4 6]) '()))
  (is (= (find-the-odd-numbers [1 1 1 3]) '(1 1 1 3))))

(deftest _fib
  (is (= (fib 3) '(1 1 2)))
  (is (= (fib 6) '(1 1 2 3 5 8)))
  (is (= (fib 8) '(1 1 2 3 5 8 13 21))))

(deftest _palindrome?
  (is (false? (palindrome? '(1 2 3 4 5))))
  (is (true? (palindrome? "racecar")))
  (is (true? (palindrome? [:foo :bar :foo])))
  (is (true? (palindrome? '(1 1 3 3 1 1))))
  (is (false? (palindrome? '(:a :b :c)))))

(deftest _flatten-seq
  (is (= (flatten-seq '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6)))
  (is (= (flatten-seq ["a" ["b"] "c"]) '("a" "b" "c")))
  (is (= (flatten-seq '((((:a))))) '(:a))))
