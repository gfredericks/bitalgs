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
                                        (* (- 3 i) 8)))))

(defn word32? [x] (instance? Word32 x))

(defn word32-with-provenance
  [long-val op-name inputs]
  (with-meta
    (if (word32? long-val)
      long-val
      (->Word32 long-val))
    {::provenance {:op-name op-name
                   :inputs inputs}
     ::id (gensym "word32")}))

(defn word32-with-id
  [long-val]
  (with-meta (->Word32 long-val)
    {::id (gensym "word32")}))

(defn mask
  [x]
  (core/bit-and 4294967295 x))

(defn bit-and
  [x y]
  (word32-with-provenance
   (core/bit-and (:long-val x)
                 (:long-val y))
   :bit-and
   [x y]))

(defn bit-not
  [x]
  (word32-with-provenance
   (core/bit-not (:long-val x))
   :bit-not
   [x]))

(defn bit-or
  [x y & zs]
  (word32-with-provenance
   (apply core/bit-or (map :long-val (list* x y zs)))
   :bit-or
   (apply vector x y zs)))

(defn bit-xor
  [x y & zs]
  (word32-with-provenance
   (apply core/bit-xor (map :long-val (list* x y zs)))
   :bit-xor
   (apply vector x y zs)))

(defn +
  [x y & zs]
  (word32-with-provenance
   (mask (apply core/+ (map :long-val (list* x y zs))))
   :+
   (apply vector x y zs)))

(defn bit-shift-left
  [x n]
  (word32-with-provenance
   (mask (core/bit-shift-left (:long-val x) n))
   :bit-shift-left
   [x n]))

(defn bit-shift-right
  [x n]
  (word32-with-provenance
   (core/bit-shift-right (:long-val x) n)
   :bit-shift-right
   [x n]))
