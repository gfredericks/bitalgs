(ns bitalgs.data.word32-test
  (:require [bitalgs.data.word32 :as word32
             :refer [->Word32]]
            [clojure.test :refer :all]))

(deftest bit-xor-test
  (are [a b c]
       (= (word32/bit-xor (->Word32 a)
                          (->Word32 b))
          (->Word32 c))

       0 0 0
       1 0 1
       0 1 1
       1 1 0
       7 7 0
       7 0 7
       7 8 15))

(deftest bit-shift-left-test
  (are [a n b]
       (= (word32/bit-shift-left (->Word32 a)
                                 n)
          (->Word32 b))

       7 1 14
       1 2 4
       489432 32 0))

(deftest +-test
  (are [a b c]
       (= (word32/+ (->Word32 a)
                    (->Word32 b))
          (->Word32 c))

       1 2 3
       7 8 15
       4294967295 1 0
       4294967295 3 2))