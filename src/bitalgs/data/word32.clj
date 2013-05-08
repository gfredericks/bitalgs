(ns bitalgs.data.word32
  (:refer-clojure :exclude [bit-xor
                            bit-or
                            bit-and
                            bit-not
                            bit-shift-left
                            bit-shift-right
                            +])
  (:require [bitalgs.data :refer [IByteString]]
            [clojure.core :as core]))

(defrecord Word32 [long-val]
  IByteString
  (bytecount [_] 4)
  (byte-at [_ i]
    (assert (<= 0 i 3))
    (core/bit-and 255
                  (core/bit-shift-right long-val
                                        (* i 8)))))

(defn word32? [x] (instance? Word32 x))

(defn mask
  [x]
  (core/bit-and 4294967295 x))

(defn bit-and
  [x y]
  (->Word32
   (core/bit-and (:long-val x)
                 (:long-val y))))

(defn bit-not
  [x]
  (->Word32 (core/bit-not (:long-val x))))

(defn bit-or
  [x y & zs]
  (->Word32
   (apply core/bit-or (map :long-val (list* x y zs)))))

(defn bit-xor
  [x y & zs]
  (->Word32
   (apply core/bit-xor (map :long-val (list* x y zs)))))

(defn +
  ([x y]
     (->Word32 (mask (core/+ (:long-val x)
                             (:long-val y)))))
  ([x y z & ws]
     (reduce + (list* x y z ws))))

(defn bit-shift-left
  [x n]
  (->Word32 (mask (core/bit-shift-left (:long-val x) n))))

(defn bit-shift-right
  [x n]
  (->Word32 (core/bit-shift-right (:long-val x) n)))