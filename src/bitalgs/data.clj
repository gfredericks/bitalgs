(ns bitalgs.data)

(defprotocol IByteString
  (bytecount [x])
  (byte-at [x i]))

(defrecord Byte [val]
  IByteString
  (bytecount [_] 1)
  (byte-at [_ i]
    (assert (zero? i))
    val))

(defn byte? [x] (instance? Byte x))

(def bytes?
  #(= ::bytes (type %)))

(def word32s?
  #(= ::word32s (type %)))

(defn to-byte
  [bs]
  {:pre [(= 8 (bitcount bs))]}
  )

(defn to-bytes
  [bs]
  {:pre [(zero? (rem (bitcount bs) 8))]}
  (vec )
  )
