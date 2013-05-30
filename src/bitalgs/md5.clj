(ns bitalgs.md5
  "http://www.kleinschmidt.com/edi/md5.htm"
  (:require [bitalgs.data :as data]
            [bitalgs.data.word32
             :as w32
             :refer [word32? bitly]]))

(def type-hierarchy
  (-> (make-hierarchy)))

(defn word64->bytes
  [x]
  ;; little-endian
  (for [i (range 0 64 8)]
    (-> x
        (bit-shift-right i)
        (bit-and 255))))

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

(defn prepare-message
  [bytes]
  (->> bytes
       (pad-message)
       (partition 4)
       (map-indexed (fn [i bs]
                      (w32/bytes->word32
                       ;; reverse so we get little-endian
                       (reverse bs)
                       ::t i
                       :type ::input)))))

(defn constant
  [hex & meta-args]
  (apply w32/word32 (Long/parseLong hex 16) meta-args))

(def init-state
  (mapv
   (fn [i name s]
     (constant s :type name ::i i))
   (range)
   [::init-A ::init-B ::init-C ::init-D]
   ["67452301"
    "EFCDAB89"
    "98BADCFE"
    "10325476"]))

(defn byte? [x] (<= 0 x 255))

(defn word32s?
  [word-nums x]
  (and (= word-nums (count x))
       (every? word32? x)))

(defn F
  [X Y Z]
  (bitly
   (or (and X Y) (and (not X) Z))))

(defn G
  [X Y Z]
  (bitly
   (or (and X Z) (and Y (not Z)))))

(defn H
  [X Y Z]
  (bitly
   (xor X Y Z)))

(defn I
  [X Y Z]
  (bitly
   (xor Y (or X (not Z)))))

;; Note that the spec speaks of T indexed by 1..64 while we're using a
;; vector here and so assuming 0..63. This corresponds to the (dec i)
;; used in the func function below.
(def T
  (mapv
   (fn [i]
     (w32/->Word32
      (long (* (Math/abs (Math/sin i))
               4294967296))))
   (range 1 65)))

(def round-specs
  ;; Copied verbatim from spec
  '[[ABCD  0  7  1]  [DABC  1 12  2]  [CDAB  2 17  3]  [BCDA  3 22  4]
    [ABCD  4  7  5]  [DABC  5 12  6]  [CDAB  6 17  7]  [BCDA  7 22  8]
    [ABCD  8  7  9]  [DABC  9 12 10]  [CDAB 10 17 11]  [BCDA 11 22 12]
    [ABCD 12  7 13]  [DABC 13 12 14]  [CDAB 14 17 15]  [BCDA 15 22 16]

    [ABCD  1  5 17]  [DABC  6  9 18]  [CDAB 11 14 19]  [BCDA  0 20 20]
    [ABCD  5  5 21]  [DABC 10  9 22]  [CDAB 15 14 23]  [BCDA  4 20 24]
    [ABCD  9  5 25]  [DABC 14  9 26]  [CDAB  3 14 27]  [BCDA  8 20 28]
    [ABCD 13  5 29]  [DABC  2  9 30]  [CDAB  7 14 31]  [BCDA 12 20 32]

    [ABCD  5  4 33]  [DABC  8 11 34]  [CDAB 11 16 35]  [BCDA 14 23 36]
    [ABCD  1  4 37]  [DABC  4 11 38]  [CDAB  7 16 39]  [BCDA 10 23 40]
    [ABCD 13  4 41]  [DABC  0 11 42]  [CDAB  3 16 43]  [BCDA  6 23 44]
    [ABCD  9  4 45]  [DABC 12 11 46]  [CDAB 15 16 47]  [BCDA  2 23 48]

    [ABCD  0  6 49]  [DABC  7 10 50]  [CDAB 14 15 51]  [BCDA  5 21 52]
    [ABCD 12  6 53]  [DABC  3 10 54]  [CDAB 10 15 55]  [BCDA  1 21 56]
    [ABCD  8  6 57]  [DABC 15 10 58]  [CDAB  6 15 59]  [BCDA 13 21 60]
    [ABCD  4  6 61]  [DABC 11 10 62]  [CDAB  2 15 63]  [BCDA  9 21 64]])

;; Here we're deliberately reinterpreting the spec, which speaks of
;; updating the variables in place according to the ABCD pattern given
;; above. Instead because the letters just rotate in the same way on
;; each iteration, I changed the style to be more similar to SHA1,
;; which should be equivalent (at least when the 64 ABCD patterns
;; above are assumed. In the general case they are not).
(defn func
  [X [A B C D] t]
  (let [f ([F G H I] (quot t 16))
        [_ k s i] (round-specs t)
        ;; decrement i so it's in the 0..63 range instead of 1..64
        i (dec i)]
    (bitly
     [D
      (+ B (<<< (+ A (f B C D) (X k) (T i)) s))
      B
      C])))

(defn state->
  [state X]
  {:pre [(word32s? 4 state)
         (word32s? 16 X)]
   :post [(word32s? 4 %)]}
  (let [[AA BB CC DD] state
        [A B C D] (reduce (partial func (vec X)) state (range 64))]
    (bitly
     ;; Putting the output metadata here rather than in the sha1
     ;; function makes the implicit assumption that we're not
     ;; going to be doing graphs for more than one chunk.
     [^::output-A, ^{::t 64, ::i 0} (+ A AA)
      ^::output-B, ^{::t 64, ::i 1} (+ B BB)
      ^::output-C, ^{::t 64, ::i 2} (+ C CC)
      ^::output-D, ^{::t 64, ::i 3} (+ D DD)])))

(defn md5-words
  "Returns a sequence of words"
  [bytes]
  (->> bytes
       (prepare-message)
       ;; chunks
       (partition 16)
       (reduce state-> init-state)))

(defn md5
  "Returns a sequence of bytes"
  [bytes]
  (mapcat (comp reverse data/bytes) (md5-words bytes)))
