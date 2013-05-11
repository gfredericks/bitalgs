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
