(ns bitalgs.graphviz
  (:require [clojure.string :as s]))

(defn prop-str
  [m]
  (s/join ","
          (for [[k v] m]
            (format "%s=\"%s\""
                    (name k) (str v)))))

(defn dot
  [{:keys [edges
           nodes
           props
           node-props]}]
  (with-out-str
    (println "digraph G {")
    (printf "node [%s];\n" (prop-str node-props))
    (doseq [{:keys [id props]} nodes]
      (printf "%s [%s];\n" id (prop-str props)))
    (doseq [{:keys [from to props]} edges]
      (printf "%s -> %s[%s];\n" from to
              (prop-str props)))
    (println "}")))
