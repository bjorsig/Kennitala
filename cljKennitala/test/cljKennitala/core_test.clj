(ns cljKennitala.core-test
  (:require [clojure.test :refer :all]
            [cljKennitala.core :refer :all]))

(deftest kennitala-test
  (testing "Kennitala should be able to tell if it's valid"
    (is (= (isValid "1111111119") true))
    (is (= (isValid "6503760649") true))
  )
  (testing "Kennitala should be able to tell if it's invalid"
    (is (= (isValid "010101") false))
    (is (= (isValid "abcdefghij") false))
    (is (= (isValid "1709715049") false))
  )
)
