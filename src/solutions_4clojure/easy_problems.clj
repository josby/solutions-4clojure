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
(defn reverse-seq [xs]
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
; Special Restrictions: flatten
; (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
; (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
; (= (__ '((((:a))))) '(:a))
(defn flatten-seq [xs]
      (letfn [(flattify [acc, x]
            (if (sequential? x)
              (reduce flattify acc x)
              (conj acc x)))]

    (reduce flattify [] xs)))


; 29 - Get the Caps
; https://www.4clojure.com/problem/29
;
; Write a function which takes a string and returns a new string containing only the capital letters.
; (= (__ "HeLlO, WoRlD!") "HLOWRD")
; (empty? (__ "nothing"))
; (= (__ "$#A(*&987Zf") "AZ")
(defn get-the-caps [input-string]
  (let [only-uppercase-char-seq (filter #(Character/isUpperCase %) input-string)]
    (apply str only-uppercase-char-seq)))


; 30 - Compress a Sequence
; https://www.4clojure.com/problem/30
;
; Write a function which removes consecutive duplicates from a sequence.
; (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
; (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
; (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))
(defn compress-seq [input]
  (letfn [(take-needed-elements [acc current]
            (if (= current (last acc))
              acc
              (conj acc current)))]
    (reduce take-needed-elements [] input)))


; 31 - Pack a Sequence
; https://www.4clojure.com/problem/31
;
; Write a function which packs consecutive duplicates into sub-lists.
; (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
; (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
; (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))
(defn pack-seq [input]
  (partition-by identity input))


; 32 - Duplicate a Sequence
; https://www.4clojure.com/problem/32
;
; Write a function which duplicates each element of a sequence.
; (= (__ [1 2 3]) '(1 1 2 2 3 3))
; (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
; (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
(defn duplicate-seq [xs]
  (->> xs
       (map #(list %, %))
       (reduce concat)))


; 33 - Replicate a Sequence
; https://www.4clojure.com/problem/33
;
; Write a function which replicates each element of a sequence a variable number of times.
; (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
; (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
; (= (__ [4 5 6] 1) '(4 5 6))
; (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
; (= (__ [44 33] 2) [44 44 33 33])
(defn replicate-seq [initial-elements n]
  (mapcat #(repeat n %) initial-elements))


; 34 - Implement range
; https://www.4clojure.com/problem/34
;
; Write a function which creates a list of all integers in a given range.
; Special Restrictions: range
; (= (__ 1 4) '(1 2 3))
; (= (__ -2 2) '(-2 -1 0 1))
; (= (__ 5 8) '(5 6 7))
(defn my-range [low high]
  (letfn [(gen-range [current]
            (lazy-seq (cons current (gen-range (inc current)))))]
    (when (< low high)
      (take-while #(not= % high) (gen-range low)))))


; 38 - Maximum value
; https://www.4clojure.com/problem/38
;
; Write a function which takes a variable number of parameters and returns the maximum value.
; (= (__ 1 8 3 4) 8)
; (= (__ 30 20) 30)
; (= (__ 45 67 11) 67)
(defn my-max [& xs]
  (reduce (fn max-of-two [left right]
            (if (> left right)
              left
              right)) xs))


; 39 - Interleave Two Seqs
; https://www.4clojure.com/problem/39
;
; Write a function which takes two sequences and returns the first item from each, then the second item from each,
; then the third, etc.
; Special Restrictions: interleave
; (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
; (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
; (= (__ [1 2 3 4] [5]) [1 5])
; (= (__ [30 20] [25 15]) [30 25 20 15])
(defn interleave-two-seqs [xs1 xs2]
  (letfn [(generate-seq [xs1 xs2]
            (lazy-seq (cons (list (first xs1) (first xs2)) (generate-seq (rest xs1) (rest xs2)))))]
    (flatten (take (min (count xs1) (count xs2)) (generate-seq xs1 xs2)))))


; 40 - Interpose a Seq
; https://www.4clojure.com/problem/40
;
; Write a function which separates the items of a sequence by an arbitrary value.
; Special Restrictions: interpose
; (= (__ 0 [1 2 3]) [1 0 2 0 3])
; (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
; (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])
(defn interpose-seq [separator xs]
  (let [tail (mapcat #(list separator %) (rest xs)) ;we do not want to add separator after the last element
        head (first xs)]
    (conj tail head )))


; 41 - Drop Every Nth Item
; https://www.4clojure.com/problem/41
;
; Write a function which drops every Nth item from a sequence.
; (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
; (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
; (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])
(defn drop-every-nth [xs n]
  (->> (partition-all n xs)
       (mapcat #(if (< (count %) n)
                  %
                  (butlast %)))))


; 42 - Factorial Fun
; https://www.4clojure.com/problem/42
;
; Write a function which calculates factorials.
; (= (__ 1) 1)
; (= (__ 3) 6)
; (= (__ 5) 120)
; (= (__ 8) 40320)
(defn factorial [n] (reduce * (range 1 (inc n))))


; 45 - Intro to Iterate
; https://www.4clojure.com/problem/45
;
; The iterate function can be used to produce an infinite lazy sequence.
; (= __ (take 5 (iterate #(+ 3 %) 1)))
(defn intro-to-iterate []
  '(1 4 7 10 13))


; 47 - Contain Yourself
; https://www.4clojure.com/problem/47
; The contains? function checks if a KEY is present in a given collection. This often leads beginner clojurians
; to use it incorrectly with numerically indexed collections like vectors and lists.
; (contains? #{4 5 6} __)
; (contains? [1 1 1 1 1] __)
; (contains? {4 :a 2 :b} __)
; (not (contains? [1 2 4] __))
(defn contain-yourself []
  4)


; 48 - Intro to some
; https://www.4clojure.com/problem/48
;
; The some function takes a predicate function and a collection.
; It returns the first logical true value of (predicate x) where x is an item in the collection.
; (= __ (some #{2 7 6} [5 6 7 8]))
; (= __ (some #(when (even? %) %) [5 6 7 8]))
(defn intro-to-some [] 6)


; 49 - Split a sequence
; https://www.4clojure.com/problem/49
;
; Write a function which will split a sequence into two parts.
; (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
; (= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
; (= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])
(defn split-seq-at [n xs]
  [(take n xs)
   (drop n xs)])


; 51 - Advanced Destructuring
; https://www.4clojure.com/problem/51
;
; Here is an example of some more sophisticated destructuring.
; (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))
(defn advanced-destructuring []
  [1 2 3 4 5])


; 62 - Re-implement Iterate
; https://www.4clojure.com/problem/62
;
; Given a side-effect free function f and an initial value x write a function which returns
; an infinite lazy sequence of x, (f x), (f (f x)), (f (f (f x))), etc.
; Special Restrictions: iterate
; (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
; (= (take 100 (__ inc 0)) (take 100 (range)))
; (= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))
(defn my-iterate [f init-val]
  (lazy-seq
   (cons init-val (my-iterate f (f init-val)))))


; 66 - Greatest Common Divisor
; https://www.4clojure.com/problem/66
;
; Given two integers, write a function which returns the greatest common divisor.
; (= (__ 2 4) 2)
; (= (__ 10 5) 5)
; (= (__ 5 7) 1)
; (= (__ 1023 858) 33)
(defn gcd [a b]
  (cond
    (= a 0) b
    (= b 0) a
    (> a b) (gcd (mod a b) b)
    (> b a) (gcd a (mod b a))))


; 81 - Set Intersection
; https://www.4clojure.com/problem/81
;
; Write a function which returns the intersection of two sets. The intersection is the sub-set
; of items that each set has in common.
; Special Restrictions: intersection
(defn set-intersection [set1 set2]
  (into #{}
        (filter set1 set2)))


; 83 - A Half-Truth
; https://www.4clojure.com/problem/83
;
; Write a function which takes a variable number of booleans. Your function should return true if
; some of the parameters are true, but not all of the parameters are true. Otherwise your function should return false.
; (= false (__ false false))
; (= true (__ true false))
; (= false (__ true))
; (= true (__ false true false))
; (= false (__ true true true))
; (= true (__ true true true false))
(defn half-truth [& booleans]
  (let [some-true (boolean (some true? booleans))
        all-true (every? identity booleans)]
    (and
     some-true
     (not all-true))))


; 88 - Symmetric Difference
; https://www.4clojure.com/problem/88
;
; Write a function which returns the symmetric difference of two sets.
; The symmetric difference is the set of items belonging to one but not both of the two sets.
; (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
; (= (__ #{:a :b :c} #{}) #{:a :b :c})
; (= (__ #{} #{4 5 6}) #{4 5 6})
; (= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})
(defn symmetric-difference [set1 set2]
  (letfn [(xor [p q]
               (and
                 (or p q)
                 (not (and p q))))]
         (let [all-elements (into set1 set2)]
              (into #{} (filter #(xor (contains? set1 %)
                                      (contains? set2 %)) all-elements)))))


; 90 - Cartesian Product
; https://www.4clojure.com/problem/90
;
; Write a function which calculates the Cartesian product of two sets.
; (= (__ #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
;     #{["ace" "♠"] ["ace" "♥"] ["ace" "♦"] ["ace" "♣"]
;       ["king" "♠"] ["king" "♥"] ["king" "♦"] ["king" "♣"]
;       ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]} )
; (= (__ #{1 2 3} #{4 5})
;     #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
; (= 300 (count (__ (into #{} (range 10))
;                   (into #{} (range 30)))))
(defn cartesian-product [left right]
  (letfn [(zip [xs current]
               (map vector (repeat current) xs))]
         (let [cartesian (mapcat (partial zip right) left)]
              (into #{} cartesian))))


; 95 - To Tree, or not to Tree
; https://www.4clojure.com/problem/95
;
; Write a predicate which checks whether or not a given sequence represents a binary tree.
; Each node in the tree must have a value, a left child, and a right child.
; (= (__ '(:a (:b nil nil) nil))
;    true)
; (= (__ '(:a (:b nil nil)))
;    false)
; (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]])
;    true)
; (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]])
;    false)
; (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil])
;    true)
; (= (__ [1 [2 [3 [4 false nil] nil] nil] nil])
;    false)
; (= (__ '(:a nil ()))
;    false)
(defn tree? [x]
  (letfn [(has-3-elements [node]
                          (= (count node) 3))]
         (cond (nil? x) true
               (or (not (sequential? x))
                   (not (has-3-elements x))) false
               :else (and (tree? (nth x 1))
                          (tree? (nth x 2))))))


; 97 - Pascal's Triangle
; https://www.4clojure.com/problem/97
;
; Write a predicate which checks whether or not a given sequence represents a binary tree.
; Pascal's triangle is a triangle of numbers computed using the following rules:
; - The first row is 1.
; - Each successive row is computed by adding together adjacent numbers in the row above, and adding a 1 to the beginning and end of the row.
; Write a function which returns the nth row of Pascal's Triangle.
; (= (__ 1) [1])
; (= (map __ (range 1 6))
;    [     [1]
;         [1 1]
;        [1 2 1]
;       [1 3 3 1]
;      [1 4 6 4 1]])
; (= (__ 11)
;    [1 10 45 120 210 252 210 120 45 10 1])
(defn get-pascal-triangle [row]
  (letfn [(calculate-value-at [row col]
            (cond
              (= col 0) 1
              (= col row) 1
              :else (+ (calculate-value-at (dec row) (dec col))
                       (calculate-value-at (dec row) col))))]
    (let [_row (dec row); _row should be 1 based
          calculated-row (for [col (range row)]
                           (calculate-value-at _row col))]
      calculated-row)))


; 99 - Product Digits
; https://www.4clojure.com/problem/99
;
; Write a function which multiplies two numbers and returns the result
; as a sequence of its digits.
; (= (__ 1 1) [1])
; (= (__ 99 9) [8 9 1])
; (= (__ 999 99) [9 8 9 0 1])
(defn product-digits [a b]
  (let [multiplied (* a b)]
    multiplied
    (loop [digits []
           current-val multiplied]
      (if (zero? current-val)
        (reverse digits)
        (recur (conj digits (mod current-val 10))
               (quot current-val 10))))))


; 107 - Simple closures
; https://www.4clojure.com/problem/107
;
; Lexical scope and first-class functions are two of the most basic building blocks of a functional language like
; Clojure. When you combine the two together, you get something very powerful called lexical closures. With these,
; you can exercise a great deal of control over the lifetime of your local bindings, saving their values for use later,
; long after the code you're running now has finished.
; It can be hard to follow in the abstract, so let's build a simple closure. Given a positive integer n,
; return a function (f x) which computes xn. Observe that the effect of this is to preserve the value of n for use
; outside the scope in which it is defined.
; (= 256 ((__ 2) 16),
;    ((__ 8) 2))
; (= [1 8 27 64] (map (__ 3) [1 2 3 4]))
; (= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4]))
(defn to-n-power [n]
  #(reduce * (repeat n %1)))


; 118 - Re-implement Map
; https://www.4clojure.com/problem/118
;
; Map is one of the core elements of a functional programming language.
; Given a function f and an input sequence s,
; return a lazy sequence of (f x) for each element x in s.
; Special Restrictions: map, map-indexed, mapcat, for
; (= [3 4 5 6 7]
;    (__ inc [2 3 4 5 6]))
; (= (repeat 10 nil)
;    (__ (fn [_] nil) (range 10)))
; (= [1000000 1000001]
;    (->> (__ inc (range))
;         (drop (dec 1000000))
;         (take 2)))
(defn my-map [f xs]
  (if (empty? xs)
    nil
    (lazy-seq
     (cons (f (first xs)) (my-map f (rest xs))))))


; 120 - Sum of square of digits
; https://www.4clojure.com/problem/120
;
; Write a function which takes a collection of integers as an argument.
; Return the count of how many elements are smaller than the sum of
; their squared component digits. For example: 10 is larger than
; 1 squared plus 0 squared; whereas 15 is smaller than 1 squared plus 5 squared.
; (= 8 (__ (range 10)))
; (= 19 (__ (range 30)))
; (= 50 (__ (range 100)))
; (= 50 (__ (range 1000)))
(defn sum-square-digits [xs]
  (letfn [(int->digits [n]
            (loop [digits '()
                   current-val n]
              (if (zero? current-val)
                digits
                (recur (conj digits (mod current-val 10))
                       (quot current-val 10)))))
          (sum-squared-digits [n]
            (reduce + (map #(* % %) (int->digits n))))]
    (count
     (filter #(< % (sum-squared-digits %)) xs))))


; 122 - Read a binary number
; https://www.4clojure.com/problem/122
;
; Convert a binary number, provided in the form of a string, to its numerical value.
; (= 0     (__ "0"))
; (= 7     (__ "111"))
; (= 7     (__ "111"))
; (= 9     (__ "1001"))
; (= 255   (__ "11111111"))
; (= 1365  (__ "10101010101"))
; (= 65535 (__ "1111111111111111"))
(defn read-binary [n]
  (letfn [(exp [base exponent]
            (reduce * (repeat exponent base)))
          (calculate-nth [idx itm]
            (if (= itm \1)
              (exp 2 idx)
              0))]

    (reduce + (map-indexed calculate-nth (reverse n)))))


; 126 - Through the Looking Class
; https://www.4clojure.com/problem/126
;
; Enter a value which satisfies the following:
;(let [x __]
;  (and (= (class x) x) x))
(defn through-the-looking-class []
  Class)


; 157 - Indexing Sequences
; https://www.4clojure.com/problem/157
;
; Transform a sequence into a sequence of pairs containing the original elements along with their index.
; (= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
; (= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
; (= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])
(defn index-seq [xs]
  (map-indexed (fn [idx e]
                 [e idx]) xs))