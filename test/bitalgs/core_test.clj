(ns bitalgs.core-test
  (:require [bitalgs.core :refer :all]
            [bitalgs.data :as data]
            [clojure.test :refer :all]))

(deftest sha1-chunk-expansion-test
  (is (= (->> (.getBytes "Message")
              (seq)
              (pad-message)
              (partition 4)
              (map bytes->word32)
              (expand-chunk)
              (map data/bytes->hex)
              (apply str))
         (str "4d65737361676580000000000000000000000000"
              "0000000000000000000000000000000000000000"
              "0000000000000000000000000000000000000000"
              "000000389acae6e6c2cecb00000000703595cdcd"
              "859d9601000000e06b2b9b9a0b3b2c733595cc0d"
              "53caa1351676580600000380acae6e692cecb1bc"
              "e3c2fdf9cab712440767364f8ea6b402b2b9b866"
              "65e5f1c4997dafe12adc4a93b132b755167661c6"
              "1cb1d1aed8bd43c43c2fdf9eab7124ac1d58ff6a"
              "e1506debfdccb09f9e7e71d4907e64d89c656d3b"
              "132b739be86df64652e515cb9da26a4b70444045"
              "04a08c3a5a8401463fda942d6df9beaaf1917c8f"
              "1b579c271eeb91bdd8cfd11b3bd819e48e750f71"
              "263bbc972fdf823c1457dfe63f9aea7c506debe1"
              "ccb09ffd7e71d71ed2cab6f9498197904e00ef5e"
              "53cf085a6b0661a176bd19cc938a939b11282798"))))

(deftest sha1-test
  (are [in-ascii out-hex]
       (= out-hex (->> (.getBytes in-ascii)
                       (seq)
                       (sha1)
                       (map data/bytes->hex)
                       (apply str)))
       "Message" "68f4145fee7dde76afceb910165924ad14cf0d00"
       "" "da39a3ee5e6b4b0d3255bfef95601890afd80709"
       "garygarygary" "2c1042f01862a7b0dbc39d4e0c13fad1c27ff7ae"))
