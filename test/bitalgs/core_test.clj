(ns bitalgs.core-test
  (:require [bitalgs.core :refer :all]
            [clojure.test :refer :all]))

(deftest sha1-test
  (are [in-ascii out-hex]
       (= out-hex (->> (.getBytes in-ascii)
                       (seq)
                       (sha1)
                       (bytes->hex)))
       "Message" "68f4145fee7dde76afceb910165924ad14cf0d00"
       "" "da39a3ee5e6b4b0d3255bfef95601890afd80709"
       "garygarygary" "2c1042f01862a7b0dbc39d4e0c13fad1c27ff7ae"))
