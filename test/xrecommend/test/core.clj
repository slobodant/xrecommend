(ns xrecommend.test.core
  (:use [xrecommend.core])
  (:use [clojure.test]))

(deftest replace-me ;; FIXME: write
  (is false "No tests have been written."))

(deftest test-that-demonstrates-failure         
 (is (= 5 (+ 2 2))))

(deftest test-that-pass         
 (is (= 5 (+ 2 3))))

(deftest test-power2 (is (= 4 (power2 2))))

(def mapa1 {:kljuc 2})
(def mapa2 {:kljuc 3})

(deftest sum-squares-test (is (= {:kljuc 1.0} (sum-squares mapa1 mapa2))))

(clojure.contrib.test-is/run-tests)

(def list1 (list 1 2 3 4))
(def list2 (list 1 2 5))

(deftest update-list-test 
  (is (= (list 5 1 2 3 4) (update-list list1 list2))))

(clojure.contrib.test-is/run-tests)