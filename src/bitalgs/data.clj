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

(defprotocol ITraceableValue
  "A protocol for values that have trace information available, presumably
   on their metadata (since the trace information shouldn't affect equality)."
  (id [x] "Returns the ID for this traceable value.")
  (inputs [x] "Returns a sequence of inputs to this traceable value.")
  (operation [x] "Returns a keyword giving the operation that produced
                  this traceable value."))

(defn traceable-value? [x] (satisfies? ITraceableValue x))

(defn traceable-inputs
  [x]
  (->> x
       (inputs)
       (filter traceable-value?)))

;; TODO: Do we also want a protocol for the thing we're using :type
;; metadata for?
