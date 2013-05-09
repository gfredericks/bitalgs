(ns bitalgs.core
  (:require [bitalgs.data.word32 :as word32]))

;; step 1 -- try to implement sha1 in a straightforward way.
;;
;; To start with we're going to represent data as seqs of bytes.
;; Probably we could make a protocol for this eh?

(defn word64->bytes
  [x]
  (loop [ret (), x x, i 0]
    (if (= 8 i)
      ret
      (recur (conj ret (bit-and x 255))
             (bit-shift-right x 8)
             (inc i)))))

(defn bytes->word32
  [[a b c d :as bytes]]
  {:pre [(= 4 (count bytes))]}
  (word32/->Word32
   (+ (* a 16777216)
      (* b 65536)
      (* c 256)
      d)))

(defn pad-message
  [bytes]
  {:post [(zero? (rem (count %) 64))]}
  (let [bytes' (concat bytes [128])
        to-64 (rem (count bytes') 64)
        spacer-byte-count (if (> to-64 56)
                            (+ 56 (- 64 to-64))
                            (- 56 to-64))
        bytes'' (concat bytes' (repeat spacer-byte-count 0))]
    (concat bytes'' (word64->bytes (* 8 (count bytes))))))

(defn constant
  [hex]
  (word32/word32-with-id (Long/parseLong hex 16)))

(def sha1-init-state
  (map constant
       ["67452301"
        "EFCDAB89"
        "98BADCFE"
        "10325476"
        "C3D2E1F0"]))

(def K-constants
  (mapv constant
        ["5A827999"
         "6ED9EBA1"
         "8F1BBCDC"
         "CA62C1D6"]))

(defn byte? [x] (<= 0 x 255))

(defn word32?
  [x]
  (word32/word32? x))

(defn word32s?
  [word-nums x]
  (and (= word-nums (count x))
       (every? word32? x)))

(defn word32-bit-rotate-left
  [x n]
  {:pre [(word32? x)]}
  (word32/word32-with-provenance
   (word32/+
    (word32/bit-shift-left x n)
    (word32/bit-shift-right x (- 32 n)))
   :bit-rotate-left
   [x n]))

(defn expand-chunk
  [chunk]
  {:pre [(word32s? 16 chunk)]
   :post [(word32s? 80 %)]}
  (loop [chunk (vec chunk), t 16]
    (if (= 80 t)
      chunk
      (let [new-word (word32-bit-rotate-left
                      (word32/bit-xor
                       (chunk (- t 3))
                       (chunk (- t 8))
                       (chunk (- t 14))
                       (chunk (- t 16)))
                      1)]
        (recur (conj chunk new-word) (inc t))))))

(defn sha1-f
  [t B C D]
  (cond (<= 0 t 19)
        (word32/bit-or
         (word32/bit-and B C)
         (word32/bit-and (word32/bit-not B) D))

        (or (<= 20 t 39)
            (<= 60 t 79))
        (word32/bit-xor B C D)

        (<= 40 59)
        (word32/bit-or
         (word32/bit-and B C)
         (word32/bit-and B D)
         (word32/bit-and C D))))

(defn sha1-K
  [t]
  (K-constants (quot t 20)))

(defn sha1-chunk
  [state chunk]
  {:pre [(word32s? 5 state)
         (word32s? 16 chunk)]
   :post [(word32s? 5 %)]}
  (let [chunk' (expand-chunk chunk)
        [H0 H1 H2 H3 H4] state]
    (loop [[A B C D E] state, t 0]
      (if (= 80 t)
        [(word32/+ A H0)
         (word32/+ B H1)
         (word32/+ C H2)
         (word32/+ D H3)
         (word32/+ E H4)]
        (let [A' (word32/+
                  (word32-bit-rotate-left A 5)
                  (sha1-f t B C D)
                  E
                  (chunk' t)
                  (sha1-K t))
              C' (word32-bit-rotate-left B 30)]
          (recur [A' A C' C D] (inc t)))))))

(defn sha1
  "Returns a sequence of words"
  [bytes]
  (->> bytes
       (pad-message)
       ;; switch from bytes to words
       (partition 4)
       (map bytes->word32)
       ;; chunks
       (partition 16)
       (reduce sha1-chunk sha1-init-state)))
