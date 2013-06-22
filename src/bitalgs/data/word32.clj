(ns bitalgs.data.word32
  (:refer-clojure :exclude [bit-xor bit-or bit-and bit-not
                            bit-shift-left bit-shift-right +])
  (:require [bitalgs.data :refer [IByteString ITraceableValue]]
            [clojure.core :as core]
            [clojure.walk :as wk]))

(defrecord Word32 [long-val]
  IByteString
  (bytecount [_] 4)
  (byte-at [_ i]
    (assert (<= 0 i 3))
    (core/bit-and 255
                  (core/bit-shift-right long-val
                                        (* (- 3 i) 8))))
  ITraceableValue
  (id [x] (:bitalgs/id (meta x)))
  (inputs [x]
    (-> x
        meta
        :bitalgs/provenance
        :inputs))
  (operation [x]
    (-> x
        meta
        :bitalgs/provenance
        :op-name)))

(alter-var-root #'->Word32
                (fn [func]
                  (fn [l]
                    (assert (integer? l))
                    (func l))))

;; Is this really a good idea?
(def ^:dynamic *metadata* {})

(defmacro with-data
  [m & body]
  `(binding [*metadata* (merge *metadata* ~m)] ~@body))

(defn word32
  [long-val & {:as more-meta}]
  (with-meta (->Word32 long-val)
    (merge *metadata*
           {:bitalgs/id (gensym "word32")}
           more-meta)))

(defn bytes->word32
  "Given four bytes with higher-order bytes first, returns a new
   Word32."
  [[a b c d :as bytes] & meta-keyvals]
  {:pre [(= 4 (count bytes))]}
  (apply word32
         (core/+ (* a 16777216)
                 (* b 65536)
                 (* c 256)
                 d)
         meta-keyvals))

(defn word32? [x]
  (and (instance? Word32 x)
       (instance? Long (:long-val x))))

(defmacro defop
  [op-name args expr]
  `(defn ~op-name
     [& args#]
     (let [~args args#]
       (word32 ~expr
               :bitalgs/provenance
               {:op-name ~(keyword op-name)
                :inputs (vec args#)}))))

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

(defop bit-rotate-left
  [x n]
  ;; a little haxy?
  (:long-val
   (+
    (bit-shift-left x n)
    (bit-shift-right x (- 32 n)))))


(defop rename
  [x]
  (:long-val x))

(defmacro bitly
  "Anaphoric macro: creates local bindings around body for all the
   bitwise functions so expressions can be more readable.

   Does a bunch with metadata. Any metadata besides :line or :column
   gets added to the result of the form at runtime. Any namespaced
   keyword whose value is true gets set to the :type."
  [expr]
  (let [metadata (fn [form]
                   (let [m (-> form meta (dissoc :line :column))]
                     (if-let [type (->> m
                                        keys
                                        (filter keyword?)
                                        (filter namespace)
                                        (filter #(true? (get m %)))
                                        (first))]
                       (-> m (dissoc type) (assoc :type type))
                       m)))
        ;; this should be postwalk but it loses metadata
        expr (wk/prewalk
              (fn [form]
                (if (seq? form)
                  (let [m (metadata form)]
                    (if (empty? m)
                      form
                      ;; the second vary-meta here avoids the
                      ;; stack overflow caused by using prewalk;
                      ;; see above.
                      `(vary-meta ~(vary-meta form
                                              select-keys
                                              [:line :column])
                                  merge ~m)))
                  form))
              expr)]
    `(let [~'+ +
           ~'and bit-and
           ~'or bit-or
           ~'not bit-not
           ~'xor bit-xor
           ~'<< bit-shift-left
           ~'>> bit-shift-right
           ~'<<< bit-rotate-left]
       ~expr)))
