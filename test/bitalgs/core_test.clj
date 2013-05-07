(ns bitalgs.core-test
  (:require [bitalgs.core :refer :all]
            [clojure.test :refer :all]))

(deftest sha1-test
  (are [in-ascii out-hex]
       (= out-hex (->> (.getBytes in-ascii)
                       (seq)
                       (sha1)
                       (bytes->hex)))
       "Message" "68f4145fee7dde76afceb910165924ad14cf0d00"))
