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

(alter-var-root #'->Word32
                (fn [func]
                  (fn [l]
                    (assert (integer? l))
                    (func l))))

(def ^:dynamic *metadata* {})

(defmacro with-data
  [m & body]
  `(binding [*metadata* (merge *metadata* ~m)] ~@body))

(defn word32? [x]
  (and (instance? Word32 x)
       (instance? Long (:long-val x))))

(defn op-meta
  [x form-meta op-name args]
  (with-meta x
    (merge *metadata*
           {:bitalgs/provenance {:op-name op-name
                                 :inputs args}
            :bitalgs/id (gensym "word32")}
           form-meta)))

(defn qualify
  [sym]
  (symbol (str *ns*) (str sym)))

(defn defop-helper
  [fn-name arg-forms op-name form-meta]
  `(let [args# [~@arg-forms]]
     (op-meta (->Word32 (apply ~fn-name args#))
              ~form-meta
              ~(-> op-name name keyword)
              args#)))

(defmacro defop
  [op-name & bodies]
  (let [fn-name (symbol (str op-name "*"))]
    `(do
       (defn ~fn-name
         ~@bodies)
       (defmacro ~op-name
         [& args#]
         (defop-helper
           '~(qualify fn-name)
           args#
           '~op-name
           (meta ~'&form))))))

(defn word32-with-provenance
  [long-val op-name inputs]
  (with-meta
    (if (word32? long-val)
      long-val
      (->Word32 long-val))
    (merge {:bitalgs/provenance {:op-name op-name
                                 :inputs inputs}
            :bitalgs/id (gensym "word32")}
           *metadata*)))

(defn word32-with-id
  [long-val & {:as attrs}]
  (with-meta (->Word32 long-val)
    (merge {:bitalgs/id (gensym "word32")}
           *metadata*
           attrs)))

(defn mask
  [x]
  (core/bit-and 4294967295 x))

(defop bit-and
  [x y]
  (core/bit-and (:long-val x)
                (:long-val y)))

(defop bit-not
  [x]
  (core/bit-not (:long-val x)))

(defop bit-or
  [x y & zs]
  (apply core/bit-or (map :long-val (list* x y zs))))

(defop bit-xor
  [x y & zs]
  (apply core/bit-xor (map :long-val (list* x y zs))))

(defop +
  [x y & zs]
  (mask (apply core/+ (map :long-val (list* x y zs)))))

(defop bit-shift-left
  [x n]
  (mask (core/bit-shift-left (:long-val x) n)))

(defop bit-shift-right
  [x n]
  (core/bit-shift-right (:long-val x) n))

(defop rename
  [x]
  (:long-val x))
