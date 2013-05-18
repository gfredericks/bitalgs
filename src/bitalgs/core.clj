(ns bitalgs.core
  (:require [bitalgs.data :refer [bytes->hex]]
            [bitalgs.graphviz :as gv]
            [bitalgs.sha1 :refer [sha1] :as sha1]
            [bitalgs.data.word32 :as w32]
            [bitalgs.util :refer [defmethods]]
            [clojure.string :as s]
            #_[clojure.core.logic :as l]
            #_[clojure.core.logic.fd :as fd]
            [com.gfredericks.svg-wrangler :refer [svg*]]
            [hiccup.core :refer [html]]))

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
         cat ::sha1/type
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
        (group-by (comp ::sha1/type meta) words)]
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

(def period 4)

;; should this one just be a giant case?
(defmulti coords (fn [x t] (::sha1/type (meta x))))

(defmethods coords [word t]

  :f-result
  [5 (+ 3 (* period t))]

  :f1
  [4 (+ 2 (* period t))]

  :f2
  [5 (+ 2 (* period t))]

  :f3
  [4 (+ 1 (* period t))]

  :f4
  [4 (+ 2 (* period t))]

  :f5
  [5 (+ 2 (* period t))]

  :f6
  [6 (+ 2 (* period t))]

  :input
  [0 (* period t)]

  :expansion
  [0 (* period t)]

  :expansion'
  [0 (dec (* period t))]

  :A
  [2 (* period t)]

  :A'
  [2 (dec (* period t))]

  :C
  [4 (* period t)]

  :K
  [7 0]

  :init-state
  [7 1]

  :output
  [1 (* period t)])

(def h (-> (make-hierarchy)
           (atom)))

(defmulti arrow-joints
  "Where the arrows points w1->w2"
  (fn [w1 w2 w1-coords w2-coords]
    [(::sha1/type w1) (::sha1/type w2)])
  :hierarchy h)

(defmethod arrow-joints :default
  [_ _ start end]
  [])

(defn prov-data->svg
  [words]
  (let [layout (into {} (for [w words] [(wordid w) (coords w (::sha1/t (meta w)))]))
        xs (->> layout vals (map first))
        ys (->> layout vals (map second))
        [minx maxx] (apply (juxt min max) xs)
        [miny maxy] (apply (juxt min max) ys)
        width (+ 2 (- maxx minx))
        height (+ 2 (- maxy miny))]
    (svg* [(dec minx)
           (dec miny)
           width
           height]
          1000
          (int (* 1000 (/ height width)))
          (list
           [:style (slurp "bitalgs.css")]
           (for [w words
                 :let [{id :bitalgs/id
                        {:keys [inputs op-name]} :bitalgs/provenance
                        type ::sha1/type
                        t ::sha1/t}
                       (meta w)

                       hex (s/upper-case (bytes->hex w))
                       [x y] (layout id)]]
             (list [:g.word
                    [:title (str (name type) ":" t)]
                    [:rect {:x (- x 0.4),
                            :y (- y 0.1),
                            :rx 0.05
                            :ry 0.05
                            :width 0.8,
                            :height 0.2}]
                    [:text {:x x, :y (+ y 0.05)} hex]]
                   (for [input inputs
                         :when (w32/word32? input)
                         :let [[x1 y1 :as p1] (layout (wordid input))
                               [x2 y2 :as p2] [x y]
                               joints (arrow-joints input w p1 p2)]]
                     [:g.word-arrow
                      [:polyline {:points (s/join " "
                                                  (for [[x y] (concat [p1] joints [p2])]
                                                    (str x "," y)))}]])))))))

(let [words (->> (.getBytes "Message")
                 (seq)
                 (sha1)
                 (provenance-data))]
  (spit "sha1.svg"
        (html (prov-data->svg words))))

(comment

  (->> (.getBytes "Message")
       (seq)
       (sha1)
       (provenance-data)
       (prov-data->graph)
       (gv/dot)
       (spit "/home/gary/public/sha1.dot"))

  (time
   (let [vs (repeatedly 20 l/lvar)]
     (l/run 1 [q]
            (l/== q vs)
            (l/everyg (fn [v] (fd/in v (fd/interval 1 2)))
                      vs))))

  (l/run 1 [q]
         (l/fresh [x1 x2 x3 x4 x5 x6 x7 x8 x9
                   x10 x11 x12 x13 x14 x15
                   x16 x17 x18 x19 x20]
                  (l/== q [x1 x2 x3 x4 x5 x6 x7 x8 x9
                           x10 x11 x12 x13 x14 x15
                           x16 x17 x18 x19 x20])
                  (fd/in x1 x2 x3 x4 x5 x6 x7 x8 x9
                         x10 x11 x12 x13 x14 x15
                         x16 x17 x18 x19 x20
                         (fd/interval 1 2))))

  )

(comment
  (defn prov-data->layout
    [words]
    (time
     (let [vars (into {} (for [w words] [(wordid w) (l/lvar)]))]
       (l/run 1 [q]
              (l/== q (vals vars))
              (l/everyg (fn [[w inputs v]]
                          (l/fresh [x y]
                                   (fd/in x y (fd/interval 1 10000))
                                   (l/== v [x y])
                                   #_(l/everyg
                                      (fn [input-word]
                                        (let [v (vars (wordid input-word))]
                                          (l/fresh [x' y']
                                                   (l/== v [x' y'])
                                                   (fd/> y y'))))
                                      inputs)))
                        (for [w words]
                          [w (filter w32/word32?
                                     (-> w meta :bitalgs/provenance :inputs))
                           (vars (wordid w))])))))))
