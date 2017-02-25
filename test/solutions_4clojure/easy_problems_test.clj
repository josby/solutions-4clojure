(ns solutions-4clojure.easy-problems-test
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

(deftest _get-the-caps
  (is (= (get-the-caps "HeLlO, WoRlD!") "HLOWRD"))
  (is (empty? (get-the-caps "nothing")))
  (is (= (get-the-caps "$#A(*&987Zf") "AZ")))

(deftest _compress-seq
  (is (= (apply str (compress-seq "Leeeeeerrroyyy")) "Leroy"))
  (is (= (compress-seq [1 1 2 3 3 2 2 3]) '(1 2 3 2 3)))
  (is (= (compress-seq [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))))

(deftest _pack-seq
  (is (= (pack-seq [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3))))
  (is (= (pack-seq [:a :a :b :b :c]) '((:a :a) (:b :b) (:c))))
  (is (= (pack-seq [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))))

(deftest _duplicate-seq
  (is (= (duplicate-seq [1 2 3]) '(1 1 2 2 3 3)))
  (is (= (duplicate-seq [:a :a :b :b]) '(:a :a :a :a :b :b :b :b)))
  (is (= (duplicate-seq [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))))

(deftest _replicate-seq
  (is (= (replicate-seq [1 2 3] 2) '(1 1 2 2 3 3)))
  (is (= (replicate-seq [:a :b] 4) '(:a :a :a :a :b :b :b :b)))
  (is (= (replicate-seq [4 5 6] 1) '(4 5 6)))
  (is (= (replicate-seq [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4])))
  (is (= (replicate-seq [44 33] 2) [44 44 33 33])))

(deftest _my-range
  (is (= (my-range 1 4) '(1 2 3)))
  (is (= (my-range -2 2) '(-2 -1 0 1)))
  (is (= (my-range 5 8) '(5 6 7))))

(deftest _my-range
  (is (= (my-range 1 4) '(1 2 3)))
  (is (= (my-range -2 2) '(-2 -1 0 1)))
  (is (= (my-range 5 8) '(5 6 7))))

(deftest _my-max
  (is (= (my-max 1 8 3 4) 8))
  (is (= (my-max 30 20) 30))
  (is (= (my-max 45 67 11) 67)))

(deftest _interleave-two-seqs
  (is (= (interleave-two-seqs [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c)))
  (is (= (interleave-two-seqs [1 2] [3 4 5 6]) '(1 3 2 4)))
  (is (= (interleave-two-seqs [1 2 3 4] [5]) [1 5]))
  (is (= (interleave-two-seqs [30 20] [25 15]) [30 25 20 15])))

(deftest _interpose-seq
  (is (= (interpose-seq 0 [1 2 3]) [1 0 2 0 3]))
  (is (= (apply str (interpose-seq ", " ["one" "two" "three"])) "one, two, three"))
  (is (= (interpose-seq :z [:a :b :c :d]) [:a :z :b :z :c :z :d])))

(deftest _drop-every-nth
  (is (= (drop-every-nth [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8]))
  (is (= (drop-every-nth [1 2 3 4 5 6] 4) [1 2 3 5 6])))
  (is (= (drop-every-nth [:a :b :c :d :e :f] 2) [:a :c :e]))
