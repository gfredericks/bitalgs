(ns bitalgs.core
  (:require [bitalgs.data :refer [bytes->hex]]
            [bitalgs.graphviz :as gv]
            [bitalgs.sha1 :refer [sha1]]
            [bitalgs.data.word32 :as w32]
            [clojure.string :as s]))

(def wordid (comp :bitalgs/id meta))

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

(defn op-label
  [kw & numeric-args]
  (case kw
    :+ "+"
    :bit-or "\u2228"
    :bit-xor "\u2295"
    :bit-and "\u2227"
    :bit-not "!"
    :bit-rotate-left (format "\\<\\<[%d]" (first numeric-args))))

(defn word-node
  [w]
  (let [{id :bitalgs/id,
         cat :category
         {:keys [inputs op-name]} :bitalgs/provenance}
        (meta w)

        hexed (s/upper-case (bytes->hex w))

        numeric-inputs (filter number? inputs)

        label (if op-name
                (format "{ %s | %s }"
                        (apply op-label op-name numeric-inputs)
                        hexed)
                hexed)]
    {:id id
     :props {:label label
             :style "filled"
             :fillcolor (case cat
                          :constant "#EE8888"
                          :input "#8888EE"
                          :output "#88EE88"
                          "white")}}))

(defn prov-data->graph*
  [words]
  (apply merge-with into
         (for [w words
               :let [{id :bitalgs/id
                      {:keys [inputs op-name]} :bitalgs/provenance}
                     (meta w)
                     node (word-node w)]]
           {:nodes [node]
            :edges (for [input inputs
                         :when (w32/word32? input)]
                     {:from (wordid input)
                      :to id})})))

(defn prov-data->graph
  [words]
  (let [grouped
        (group-by (comp :category meta) words)]
    (merge (prov-data->graph* words)
           {:node-props {:shape "record"
                         :style "rounded"}
            :graphs (for [category [:constant :input :output]
                          :let [props ({:constant {}
                                        :input {:rank "source"}
                                        :output {:rank "sink"}}
                                       category)]]
                      {:props props
                       :nodes (map word-node
                                   (grouped category))})})))

(comment
  (->> (.getBytes "Message")
       (seq)
       (sha1)
       (provenance-data)
       (prov-data->graph)
       (gv/dot)
       (spit "/home/gary/public/sha1.dot"))
  )
