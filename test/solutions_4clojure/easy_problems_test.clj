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
  (is (= (drop-every-nth [1 2 3 4 5 6] 4) [1 2 3 5 6]))
  (is (= (drop-every-nth [:a :b :c :d :e :f] 2) [:a :c :e])))

(deftest _factorial
  (is (= (factorial 1) 1))
  (is (= (factorial 3) 6))
  (is (= (factorial 5) 120))
  (is (= (factorial 8) 40320)))

(deftest _intro-to-iterate
  (is (= (intro-to-iterate) (take 5 (iterate #(+ 3 %) 1)))))

(deftest _contain-yourself
  (is (contains? #{4 5 6} (contain-yourself)))
  (is (contains? [1 1 1 1 1] (contain-yourself)))
  (is (contains? {4 :a 2 :b} (contain-yourself)))
  (is (not (contains? [1 2 4] (contain-yourself)))))

(deftest _intro-to-some
  (is (= (intro-to-some) (some #{2 7 6} [5 6 7 8])))
  (is (= (intro-to-some) (some #{2 7 6} [5 6 7 8]))))

(deftest _split-seq-at
  (is (= (split-seq-at 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]]))
  (is (= (split-seq-at 1 [:a :b :c :d]) [[:a] [:b :c :d]]))
  (is (= (split-seq-at 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])))

(deftest _advanced-destructuring
  (is (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] (advanced-destructuring)] [a b c d]))))

(deftest _my-iterate
  (is (= (take 5 (my-iterate #(* 2 %) 1)) [1 2 4 8 16]))
  (is (= (take 100 (my-iterate inc 0)) (take 100 (range))))
  (is (= (take 9 (my-iterate #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))))

(deftest _gcd
  (is (= (gcd 2 4) 2))
  (is (= (gcd 10 5) 5))
  (is (= (gcd 5 7) 1))
  (is (= (gcd 1023 858) 33)))

(deftest _set-intersection
  (is (= (set-intersection #{0 1 2 3} #{2 3 4 5}) #{2 3}))
  (is (= (set-intersection #{0 1 2} #{3 4 5}) #{}))
  (is (= (set-intersection #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})))

(deftest _half-truth
  (is (= false (half-truth false false)))
  (is (= true (half-truth true false)))
  (is (= false (half-truth true)))
  (is (= true (half-truth false true false)))
  (is (= false (half-truth true true true)))
  (is (= true (half-truth true true true false))))

(deftest _symmetric-difference
  (is (= (symmetric-difference #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7}))
  (is (= (symmetric-difference #{:a :b :c} #{}) #{:a :b :c}))
  (is (= (symmetric-difference #{} #{4 5 6}) #{4 5 6}))
  (is (= (symmetric-difference #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})))

(deftest _cartesian-product
  (is (= (cartesian-product #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
         #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
           ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
           ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]}))
  (is (= (cartesian-product #{1 2 3} #{4 5})
         #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]}))
  (is (= 300 (count (cartesian-product (into #{} (range 10))
                                       (into #{} (range 30)))))))

(deftest _tree?
  (is (= (tree? '(:a (:b nil nil) nil))
         true))
  (is (= (tree? '(:a (:b nil nil)))
         false))
  (is (= (tree? [1 nil [2 [3 nil nil] [4 nil nil]]])
         true))
  (is (= (tree? [1 [2 nil nil] [3 nil nil] [4 nil nil]])
         false))
  (is (= (tree? [1 [2 [3 [4 nil nil] nil] nil] nil])
         true))
  (is (= (tree? [1 [2 [3 [4 false nil] nil] nil] nil])
         false))
  (is (= (tree? '(:a nil ()))
         false)))

(deftest _get-pascal-triangle
  (is (= (get-pascal-triangle 1) [1]))
  (is (= (map get-pascal-triangle (range 1 6))
         [ [1]
          [1 1]
         [1 2 1]
        [1 3 3 1]
       [1 4 6 4 1]]))
  (is (= (get-pascal-triangle 11)
         [1 10 45 120 210 252 210 120 45 10 1])))

(deftest _product-digits
  (is (= (product-digits 1 1) [1]))
  (is (= (product-digits 99 9) [8 9 1]))
  (is (= (product-digits 999 99) [9 8 9 0 1])))

(deftest _to-n-power
  (is (= 256 ((to-n-power 2) 16),
         ((to-n-power 8) 2)))
  (is (= [1 8 27 64] (map (to-n-power 3) [1 2 3 4])))
  (is (= [1 2 4 8 16] (map #((to-n-power %) 2) [0 1 2 3 4]))))

(deftest _my-map
  (is (= [3 4 5 6 7]
         (my-map inc [2 3 4 5 6])))
  (is (= (repeat 10 nil)
         (my-map (fn [_] nil) (range 10))))
  (is (= [1000000 1000001]
         (->> (my-map inc (range))
              (drop (dec 1000000))
              (take 2)))))

(deftest _read-binary
  (is (= 0     (read-binary "0")))
  (is (= 7     (read-binary "111")))
  (is (= 8     (read-binary "1000")))
  (is (= 9     (read-binary "1001")))
  (is (= 255   (read-binary "11111111")))
  (is (= 1365  (read-binary "10101010101")))
  (is (= 65535 (read-binary "1111111111111111"))))

(deftest _sum-square-digits
  (is (= 8 (sum-square-digits (range 10))))
  (is (= 19 (sum-square-digits (range 30))))
  (is (= 50 (sum-square-digits (range 100))))
  (is (= 50 (sum-square-digits (range 1000)))))

(deftest _index-seq
  (is (= (index-seq [:a :b :c]) [[:a 0] [:b 1] [:c 2]]))
  (is (= (index-seq [0 1 3]) '((0 0) (1 1) (3 2))))
  (is (= (index-seq [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])))
