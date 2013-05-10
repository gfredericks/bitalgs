(ns bitalgs.sha1
  (:require [bitalgs.data.word32
             :as w32
             :refer [word32?]]))

(defn word64->bytes
  [x]
  (loop [ret (), x x, i 0]
    (if (= 8 i)
      ret
      (recur (conj ret (bit-and x 255))
             (bit-shift-right x 8)
             (inc i)))))

(defn input-word
  [[a b c d :as bytes]]
  {:pre [(= 4 (count bytes))]}
  (w32/word32-with-id
   (+ (* a 16777216)
      (* b 65536)
      (* c 256)
      d)
   :category :input))

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
  (w32/word32-with-id (Long/parseLong hex 16)))

(def sha1-init-state
  (w32/with-data {:category :constant}
    (mapv constant
          ["67452301"
           "EFCDAB89"
           "98BADCFE"
           "10325476"
           "C3D2E1F0"])))

(def K-constants
  (w32/with-data {:category :constant}
    (mapv constant
          ["5A827999"
           "6ED9EBA1"
           "8F1BBCDC"
           "CA62C1D6"])))

(defn byte? [x] (<= 0 x 255))

(defn word32s?
  [word-nums x]
  (and (= word-nums (count x))
       (every? word32? x)))

(defn word32-bit-rotate-left
  [x n]
  {:pre [(word32? x)]}
  (w32/word32-with-provenance
   (w32/+
    (w32/bit-shift-left x n)
    (w32/bit-shift-right x (- 32 n)))
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
                      (w32/bit-xor
                       (chunk (- t 3))
                       (chunk (- t 8))
                       (chunk (- t 14))
                       (chunk (- t 16)))
                      1)]
        (recur (conj chunk new-word) (inc t))))))

(defn sha1-f
  [t B C D]
  (cond (<= 0 t 19)
        (w32/bit-or
         (w32/bit-and B C)
         (w32/bit-and (w32/bit-not B) D))

        (or (<= 20 t 39)
            (<= 60 t 79))
        (w32/bit-xor B C D)

        (<= 40 59)
        (w32/bit-or
         (w32/bit-and B C)
         (w32/bit-and B D)
         (w32/bit-and C D))))

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
        [(w32/+ A H0)
         (w32/+ B H1)
         (w32/+ C H2)
         (w32/+ D H3)
         (w32/+ E H4)]
        (let [A' (w32/+
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
       (map input-word)
       ;; chunks
       (partition 16)
       (reduce sha1-chunk sha1-init-state)))
