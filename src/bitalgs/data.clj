(ns bitalgs.data
  (:refer-clojure :exclude [bytes]))

(defprotocol IByteString
  (bytecount [x] "Returns the number of bytes in this byte string.")
  (byte-at [x i] "Returns a long between 0 and 255."))

(defn bytes
  [bytestring]
  (for [i (range (bytecount bytestring))]
    (byte-at bytestring i)))

(defn bytes->hex
  [bytestring]
  (apply str
         (for [b (bytes bytestring)
               :let [s (Integer/toHexString b)]]
           (if (= 2 (count s)) s (str \0 s)))))
