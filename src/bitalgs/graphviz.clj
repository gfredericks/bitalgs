(ns bitalgs.graphviz
  (:require [clojure.string :as s]))

(defn prop-str
  [m]
  (s/join ","
          (for [[k v] m]
            (format "%s=\"%s\""
                    (name k) (str v)))))

(defn print-graph
  [{:keys [edges
           nodes
           props
           graphs
           node-props]}]
  (println "{")
  (doseq [[k v] props]
    (printf "%s=%s;\n" (name k) (str v)))
  (when node-props
    (printf "node [%s];\n" (prop-str node-props)))
  (doseq [{:keys [id props]} nodes]
    (printf "%s [%s];\n" id (prop-str props)))
  (doseq [{:keys [from to props]} edges]
    (printf "%s -> %s[%s];\n" from to
            (prop-str props)))
  (doseq [g graphs]
    (print-graph g))
  (println "}"))

(defn dot
  [g]
  (with-out-str
    (print "digraph G ")
    (print-graph g)))

;;
;; Some old code for the SHA1 thing
;;

(comment
  (defn word-node
    [w]
    (let [{id :bitalgs/id,
           cat :type
           {:keys [inputs op-name]} :bitalgs/provenance}
          (meta w)

          hexed (s/upper-case (bytes->hex w))

          numeric-inputs (filter number? inputs)

          label (if op-name
                  (format "{ %s | %s }"
                          (op-label op-name numeric-inputs)
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
          (group-by (comp :type meta) words)]
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
                                     (grouped category))})}))))
