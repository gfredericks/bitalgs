(ns bitalgs.md5-test
  (:require [bitalgs.md5 :refer :all]
            [bitalgs.data :as data]
            [clojure.test :refer :all]))

(deftest md5-test
  (are [in-ascii out-hex]
       (= out-hex (->> (.getBytes in-ascii)
                       (seq)
                       (md5)
                       (map data/bytes->hex)
                       (apply str)))
       ""
       "d41d8cd98f00b204e9800998ecf8427e"

       "denny"
       "3425f115ee1ecf591fb06d635c37d990"))
