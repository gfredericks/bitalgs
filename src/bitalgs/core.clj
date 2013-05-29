(ns bitalgs.core
  (:require [bitalgs.data :refer [bytes->hex]]
            [bitalgs.data.word32 :as w32]
            [bitalgs.graphviz :as gv]
            [bitalgs.sha1 :as sha1]
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
      (let [{id :bitalgs/id
             prov :bitalgs/provenance}
            (meta x)]
        (recur (assoc ids id x)
               (->> (:inputs prov)
                    (filter w32/word32?)
                    (remove (comp ids :bitalgs/id meta))
                    (into xs))))
      (vals ids))))

(let [input-string "denny"
      words (->> (.getBytes input-string)
                 (seq)
                 (sha1/sha1-words)
                 (provenance-data))]
  (spit "sha1.svg"
        (html (svg-sha1/svg input-string words))))
