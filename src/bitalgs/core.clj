(ns bitalgs.core)

;; step 1 -- try to implement sha1 in a straightforward way.
;;
;; To start with we're going to represent data as seqs of bytes.
;; Probably we could make a protocol for this eh?

(defprotocol IBitstring
  (bitcount [_])
  (bit-at [_ i]))

(defn word64->bytes
  [x]
  (loop [ret (), x x, i 0]
    (if (= 8 i)
      ret
      (recur (conj ret (bit-and x 255))
             (bit-shift-right x 8)
             (inc i)))))

;; these names are misleading
(defn word32->bytes
  [x]
  (loop [ret (), x x, i 0]
    (if (= 4 i)
      ret
      (recur (conj ret (bit-and x 255))
             (bit-shift-right x 8)
             (inc i)))))

(defn bytes->word32
  [bytes]
  {:pre [(= 4 (count bytes))]}
  (apply +
         (map *
              (reverse bytes)
              (iterate #(* 256 %) 1))))

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

(def sha1-init-state
  (map (comp word32->bytes #(Long/parseLong % 16))
       ["67452301"
        "EFCDAB89"
        "98BADCFE"
        "10325476"
        "C3D2E1F0"]))

(defn byte? [x] (<= 0 x 255))

(defn word32?
  [x]
  (and (every? byte? x)
       (= 4 (count x))))

(defn word32s?
  [word-nums x]
  (and (= word-nums (count x))
       (every? word32? x)))

(defn word32-bit-rotate-left
  [n x]
  {:pre [(word32? x)]}
  (as-> x x
        (bytes->word32 x)
        (+ (bit-and 4294967295
                    (bit-shift-left n x))
           (bit-and 4294967295
                    (bit-shift-right (- 32 n) x)))
        (word32->bytes x)))

;; this doesn't do what i expect, so I think
;; the above is wrong.
(word32-bit-rotate-left 1 [8 0 0 0])

(defn word32-xor
  [& xs])

(defn expand-chunk
  [chunk]
  {:pre [(word32s? 16 chunk)]
   :post [(word32s? 80 %)]}
  (loop [chunk (vec chunk), t 16]
    (if (= 80 t)
      chunk
      (word32-bit-rotate-left 1
                       (word32-xor
                        (chunk (- t 3))
                        (chunk (- t 8))
                        (chunk (- t 14))
                        (chunk (- t 16)))))))

(defn sha1-chunk
  [state chunk]
  {:pre [(word32s? 5 state)
         (word32s? 16 chunk)]
   :post [(word32s? 5 %)]}

  state)

(defn sha1
  [bytes]
  (->> bytes
       (pad-message)
       ;; switch from bytes to words
       (partition 4)
       ;; chunks
       (partition 16)
       (reduce sha1-chunk sha1-init-state)
       ;; back from words to bytes
       (apply concat)))

(comment

  (sha1 (.getBytes "Message"))

  (->> (.getBytes "Message")
       (seq)
       (pad-message)
       (map #(Integer/toHexString (int %)))
       (count))

  (for [word sha1-init-state]
    (for [b word]
      (Integer/toHexString (int b))))
 )