(ns bitalgs.core)

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

(defn mask-32
  [x]
  (bit-and 4294967295 x))

(defn word32-bit-rotate-left
  [x n]
  {:pre [(word32? x)]}
  (as-> x x
        (bytes->word32 x)
        (+ (mask-32 (bit-shift-left x n))
           (mask-32 (bit-shift-right x (- 32 n))))
        (word32->bytes x)))

(defn word32-xor
  ([x] x)
  ([x1 x2]
     (map bit-xor x1 x2))
  ([x1 x2 x3 & xs]
     (reduce word32-xor (list* x1 x2 x3 xs))))

(defn word32-add
  "Adds two words mod 2^32."
  ([x] x)
  ([x y]
     (let [x' (bytes->word32 x)
           y' (bytes->word32 y)]
       (-> (+ x' y')
           (mask-32)
           (word32->bytes))))
  ([x1 x2 x3 & xz]
     (reduce word32-add (list* x1 x2 x3 xz))))

(defn word32-and
  [x y]
  (map bit-and x y))

(defn word32-not
  [x]
  (map bit-not x))

(defn word32-or
  ([x] x)
  ([x y & zs]
     (apply map bit-or x y zs)))

(defn expand-chunk
  [chunk]
  {:pre [(word32s? 16 chunk)]
   :post [(word32s? 80 %)]}
  (loop [chunk (vec chunk), t 16]
    (if (= 80 t)
      chunk
      (let [new-word (word32-bit-rotate-left
                      (word32-xor
                       (chunk (- t 3))
                       (chunk (- t 8))
                       (chunk (- t 14))
                       (chunk (- t 16)))
                      1)]
        (recur (conj chunk new-word) (inc t))))))

(defn sha1-f
  [t B C D]
  (cond (<= 0 t 19)
        (word32-or
         (word32-and B C)
         (word32-and (word32-not B) D))

        (or (<= 20 t 39)
            (<= 60 t 79))
        (word32-xor B C D)

        (<= 40 59)
        (word32-or
         (word32-and B C)
         (word32-and B D)
         (word32-and C D))))

(defn sha1-K
  [t]
  ((comp word32->bytes #(Long/parseLong % 16))
   (cond (<= 0 t 19) "5A827999"
         (<= 20 t 39) "6ED9EBA1"
         (<= 40 t 59) "8F1BBCDC"
         (<= 60 t 79) "CA62C1D6")))

(defn sha1-chunk
  [state chunk]
  {:pre [(word32s? 5 state)
         (word32s? 16 chunk)]
   :post [(word32s? 5 %)]}
  (let [chunk' (expand-chunk chunk)
        [H0 H1 H2 H3 H4] state]
    (loop [[A B C D E] state, t 0]
      (if (= 80 t)
        [(word32-add A H0)
         (word32-add B H1)
         (word32-add C H2)
         (word32-add D H3)
         (word32-add E H4)]
        (let [A' (word32-add
                  (word32-bit-rotate-left A 5)
                  (sha1-f t B C D)
                  E
                  (chunk' t)
                  (sha1-K t))
              C' (word32-bit-rotate-left B 30)]
          (recur [A' A C' C D] (inc t)))))))

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

(defn bytes->hex
  [bytes]
  (apply str
         (for [b bytes
               :let [s (Integer/toHexString b)]]
           (if (= 2 (count s)) s (str \0 s)))))

(comment

  (sha1 (.getBytes "Message"))

  (->> (.getBytes "Message")
       (seq)
       (pad-message)

       (partition 4)
       (expand-chunk)
       (apply concat)
       (map #(Integer/toHexString (int %))))

  (word32? (word32-xor '(0 0 0 0) '(0 0 0 0) '(0 0 0 0) '(77 101 115 115)))

  (for [word sha1-init-state]
    (for [b word]
      (Integer/toHexString (int b))))
 )
