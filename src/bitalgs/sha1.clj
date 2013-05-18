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

(defn assoc-meta
  [x & kvs]
  (with-meta x (apply assoc (meta x) kvs)))

(defn prepare-message
  [bytes]
  (->> bytes
       (pad-message)
       (partition 4)
       (map input-word)
       (map-indexed (fn [i w]
                      (assoc-meta w
                                  ::t i
                                  ::type :input)))))

(defn constant
  [hex]
  (w32/word32-with-id (Long/parseLong hex 16)))

(def sha1-init-state
  (w32/with-data {::type :init-state, ::t 0}
    (mapv constant
          ["67452301"
           "EFCDAB89"
           "98BADCFE"
           "10325476"
           "C3D2E1F0"])))

(def K-constants
  (w32/with-data {::type :K}
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

(w32/defop bit-rotate-left
  [x n]
  {:pre [(word32? x)]}
  ;; a little haxy?
  (:long-val
   (w32/+
    (w32/bit-shift-left x n)
    (w32/bit-shift-right x (- 32 n)))))

(defn expand-chunk
  [chunk]
  {:pre [(word32s? 16 chunk)]
   :post [(word32s? 80 %)]}
  (loop [chunk (vec chunk), t 16]
    (if (= 80 t)
      chunk
      (let [new-word
            (bit-rotate-left
             ^{::type :expansion', ::t t}
             (w32/bit-xor
              (chunk (- t 3))
              (chunk (- t 8))
              (chunk (- t 14))
              (chunk (- t 16)))
             1)
            new-word' (assoc-meta new-word
                                  ::type :expansion
                                  ::t t)]
        (recur (conj chunk new-word') (inc t))))))

(defn sha1-f
  [t B C D]
  (assoc-meta
   (cond (<= 0 t 19)
         ^{:f-type 1}
         (w32/bit-or
          ^{::type :f1}
          (w32/bit-and B C)
          ^{::type :f2}
          (w32/bit-and ^{::type :f3} (w32/bit-not B) D))

         (or (<= 20 t 39)
             (<= 60 t 79))
         ^{:f-type 2}
         (w32/bit-xor B C D)

         (<= 40 59)
         ^{:f-type 3}
         (w32/bit-or
          ^{::type :f4} (w32/bit-and B C)
          ^{::type :f5} (w32/bit-and B D)
          ^{::type :f6} (w32/bit-and C D)))
   ::type :f-result))

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
        (w32/with-data {::type :output, ::t (inc t)}
          [^{:output :A} (w32/+ A H0)
           ^{:output :B} (w32/+ B H1)
           ^{:output :C} (w32/+ C H2)
           ^{:output :D} (w32/+ D H3)
           ^{:output :E} (w32/+ E H4)])
        (let [t' (inc t)

              A'
              (w32/with-data {::t t'}
                ^{::type :A}
                (w32/+
                 ^{::type :A'}
                 (bit-rotate-left A 5)
                 (sha1-f t B C D)
                 E
                 (chunk' t)
                 (sha1-K t)))

              C' ^{::t t', ::type :C}
              (bit-rotate-left B 30)]
          (recur [A' A C' C D] (inc t)))))))

(defn sha1
  "Returns a sequence of words"
  [bytes]
  (->> bytes
       (prepare-message)
       ;; chunks
       (partition 16)
       (reduce sha1-chunk sha1-init-state)))
