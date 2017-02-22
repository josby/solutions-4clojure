(ns solutions-4clojure.easy-problems-tests
    (:require [clojure.test :refer :all]
      [solutions-4clojure.easy-problems :refer :all]))

(deftest _last-element
  (is (= (last-element [1 2 3 4 5]) 5))
  (is (= (last-element '(5 4 3)) 3))
  (is (= (last-element ["b" "c" "d"]) "d")))
