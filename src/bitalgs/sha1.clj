(ns bitalgs.sha1
  (:require [bitalgs.data.word32
             :as w32
             :refer [word32? bitly]]))

(def type-hierarchy
  (-> (make-hierarchy)
      (derive ::input ::chunks)
      (derive ::expansion ::chunks)
      (derive ::expansion' ::chunks)
      (derive ::input ::input')
      (derive ::expansion ::input')

      ;; The ::init parent class
      (derive ::init-A ::init)
      (derive ::init-B ::init)
      (derive ::init-C ::init)
      (derive ::init-D ::init)
      (derive ::init-E ::init)

      (derive ::output-A ::output)
      (derive ::output-B ::output)
      (derive ::output-C ::output)
      (derive ::output-D ::output)
      (derive ::output-E ::output)

      (derive ::init ::constant)
      (derive ::K ::constant)

      (derive ::A ::A-sup)
      (derive ::init-A ::A-sup)

      (derive ::B ::B-sup)
      (derive ::init-B ::B-sup)

      (derive ::C ::C-sup)
      (derive ::init-C ::C-sup)

      (derive ::D ::D-sup)
      (derive ::init-D ::D-sup)

      (derive ::E ::E-sup)
      (derive ::init-E ::E-sup)

      (derive ::f1 ::f)
      (derive ::f2 ::f)
      (derive ::f3 ::f)))

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
  (apply vary-meta x assoc kvs))

(defn prepare-message
  [bytes]
  (->> bytes
       (pad-message)
       (partition 4)
       (map input-word)
       (map-indexed (fn [i w]
                      (assoc-meta w
                                  ::t i
                                  :type ::input)))))

(defn constant
  [hex]
  (w32/word32-with-id (Long/parseLong hex 16)))

(def sha1-init-state
  (mapv
   (fn [i name s]
     (assoc-meta (constant s) :type name, ::i i))
   (range)
   [::init-A ::init-B ::init-C ::init-D ::init-E]
   ["67452301"
    "EFCDAB89"
    "98BADCFE"
    "10325476"
    "C3D2E1F0"]))

(def K-constants
  (mapv
   (fn [i s]
     (assoc-meta (constant s) :type ::K, ::i i))
   (range)
   ["5A827999"
    "6ED9EBA1"
    "8F1BBCDC"
    "CA62C1D6"]))

(defn byte? [x] (<= 0 x 255))

(defn word32s?
  [word-nums x]
  (and (= word-nums (count x))
       (every? word32? x)))

(defn expand-chunk
  [chunk]
  {:pre [(word32s? 16 chunk)]
   :post [(word32s? 80 %)]}
  (loop [chunk (vec chunk), t 16]
    (if (= 80 t)
      chunk
      (let [new-word
            (bitly
             ^::expansion ^{::t t}
             (<<<
              ^::expansion' ^{::t t}
              (xor (chunk (- t 3))
                   (chunk (- t 8))
                   (chunk (- t 14))
                   (chunk (- t 16)))
              1))]
        (recur (conj chunk new-word) (inc t))))))

(defn sha1-f
  [t B C D]
  (w32/with-data {::t t}
    (bitly
     (cond (<= 0 t 19)
           ^::f1 (or ^::f1a (and B C)
                     ^::f1b (and ^::f1c (not B) D))

           (clojure.core/or
            (<= 20 t 39)
            (<= 60 t 79))
           ^::f2 (xor B C D)

           (<= 40 59)
           ^::f3 (or ^::f3a (and B C)
                     ^::f3b (and B D)
                     ^::f3c (and C D))))))

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
      (bitly
       (if (= 80 t)
         ;; Putting the output metadata here rather than in the sha1
         ;; function makes the implicit assumption that we're not
         ;; going to be doing graphs for more than one chunk.
         [^::output-A, ^{::t t, ::i 0} (+ A H0)
          ^::output-B, ^{::t t, ::i 1} (+ B H1)
          ^::output-C, ^{::t t, ::i 2} (+ C H2)
          ^::output-D, ^{::t t, ::i 3} (+ D H3)
          ^::output-E, ^{::t t, ::i 4} (+ E H4)]
         (let [A'
               ^::A ^{::t t}
               (+ ^::A' ^{::t t} (<<< A 5)
                  (sha1-f t B C D)
                  E
                  (chunk' t)
                  (sha1-K t))

               C' ^::C ^{::t t} (<<< B 30)]
           (recur [A'
                   ^::B ^{::t t} (w32/rename A)
                   C'
                   ^::D ^{::t t} (w32/rename C)
                   ^::E ^{::t t} (w32/rename D)]
                  (inc t))))))))

(defn sha1
  "Returns a sequence of words"
  [bytes]
  (->> bytes
       (prepare-message)
       ;; chunks
       (partition 16)
       (reduce sha1-chunk sha1-init-state)))
