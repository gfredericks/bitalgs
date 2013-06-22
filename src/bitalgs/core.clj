(ns bitalgs.core
  (:require [bitalgs.data :refer [bytes->hex] :as data]
            [bitalgs.data.word32 :as w32]
            [bitalgs.graphviz :as gv]
            [bitalgs.md5 :as md5]
            [bitalgs.sha1 :as sha1]
            [bitalgs.svg.md5 :as svg-md5]
            [bitalgs.svg.sha1 :as svg-sha1]
            [clojure.string :as s]
            [hiccup.core :refer [html]]))

(defn provenance-data
  "Returns a sequence of Word32s"
  [words]
  {:post [(every? w32/word32? %)]}
  ;; using a map helps ensure we don't double-walk anything
  (loop [ids {},
         remaining words]
    (if-let [[x & xs] (seq remaining)]
      (recur (assoc ids (data/id x) x)
             (->> (data/traceable-inputs x)
                  (remove (comp ids data/id))
                  (into xs)))
      (vals ids))))

(comment
  (let [input-string "denny"
        words (->> (.getBytes input-string)
                   (seq)
                   (sha1/sha1-words)
                   (provenance-data))]
    (def words words))

  (let [input-string "denny"
        words (->> (.getBytes input-string)
                   (seq)
                   (sha1/sha1-words)
                   (provenance-data))]
    (spit "sha1.svg"
          (html (svg-sha1/svg input-string words))))


  (let [input-string "denny"
        words (->> (.getBytes input-string)
                   (seq)
                   (md5/md5-words)
                   (provenance-data))]
    (spit "md5.svg"
          (html (svg-md5/svg input-string words)))))
