(ns bitalgs.core
  (:require [bitalgs.data :refer [bytes->hex]]
            [bitalgs.graphviz :as gv]
            [bitalgs.sha1 :refer [sha1]]
            [bitalgs.data.word32 :as w32]
            [clojure.string :as s]))

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

(defn word-node
  [w]
  (let [{id :bitalgs/id, cat :category} (meta w)]
    {:id id
     :props {:label (s/upper-case (bytes->hex w))
             :style "filled"
             :fillcolor (case cat
                          :constant "#EE8888"
                          :input "#8888EE"
                          "white")}}))

(defn prov-data->graph
  [words]
  (apply merge-with into
         {:props {}
          :node-props {:shape "rect"
                       :style "rounded"}}
         (for [w words
               :let [{id :bitalgs/id
                      {:keys [inputs op-name]} :bitalgs/provenance}
                     (meta w)
                     node (word-node w)]]
           (if inputs
             (let [op-id (str "op" id)
                   numeric-inputs (filter number? inputs)
                   op-label (if (seq numeric-inputs)
                              (format "%s[%s]"
                                      (name op-name)
                                      (s/join "," numeric-inputs))
                              (name op-name))
                   op-node {:id op-id
                            :props {:label op-label}}
                   op-edge {:from op-id
                            :to id}
                   input-edges (for [input inputs
                                     :when (w32/word32? input)
                                     :let [{input-id :bitalgs/id}
                                           (meta input)]]
                                 {:from input-id
                                  :to op-id})]
               {:nodes [node op-node]
                :edges (apply vector op-edge input-edges)})
             {:nodes [node]}))))

(comment
  (->> (.getBytes "Message")
       (seq)
       (sha1)
       (provenance-data)
       (prov-data->graph)
       (gv/dot)
       (spit "/home/gary/public/sha1.dot"))
  )
