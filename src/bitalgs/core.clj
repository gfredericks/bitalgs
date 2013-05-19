(ns bitalgs.core
  (:require [bitalgs.data :refer [bytes->hex]]
            [bitalgs.graphviz :as gv]
            [bitalgs.sha1 :refer [sha1] :as sha1]
            [bitalgs.data.word32 :as w32]
            [bitalgs.util :refer [defmethods]]
            [clojure.string :as s]
            #_[clojure.core.logic :as l]
            #_[clojure.core.logic.fd :as fd]
            [com.gfredericks.svg-wrangler :refer [svg*] :as svg]
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
  [kw numeric-args]
  (case kw
    :+ "+"
    :bit-or "\u2228"
    :bit-xor "\u2295"
    :bit-and "\u2227"
    :bit-not "!"
    :bit-rotate-left (format "&lt;&lt;[%d]" (first numeric-args))))

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

(defn local-type
  [w]
  (->> w
       (meta)
       (::sha1/type)
       (name)
       (keyword (namespace ::foo))))

(def h (-> (make-hierarchy)
           (derive ::input ::chunks)
           (derive ::expansion ::chunks)
           (derive ::expansion' ::chunks)
           (derive ::input ::input')
           (derive ::expansion ::input')

           ;; The ::init parent class
           (derive ::init-A ::init)
           (derive ::init-B ::init)
           (derive ::init-C ::init)
           (derive ::init-D ::init)
           (derive ::init-E ::init)

           (derive ::A ::A-sup)
           (derive ::init-A ::A-sup)

           (derive ::C ::C-sup)
           (derive ::init-C ::C-sup)
           ))

(defmulti coords (fn [x t] (local-type x))
  :hierarchy #'h)

(defmethods coords [word t]

  ::f-result
  [5 (+ 3 (* period t))]

  ::f1
  [4 (+ 2 (* period t))]

  ::f2
  [5 (+ 2 (* period t))]

  ::f3
  [4 (+ 1 (* period t))]

  ::f4
  [4 (+ 2 (* period t))]

  ::f5
  [5 (+ 2 (* period t))]

  ::f6
  [6 (+ 2 (* period t))]

  ::input'
  [0 (+ 3 (* period t))]

  ::expansion'
  [0 (+ 2 (* period t))]

  ::A
  [2 (* period (inc t))]

  ::A'
  [2 (dec (* period (inc t)))]

  ::C
  [4 (* period (inc t))]

  ::K
  [9 (dec (* period 20 (::sha1/i (meta word))))]

  ::init
  [(+ 2 (::sha1/i (meta word))) -1]

  ::output
  [(+ 2 (::sha1/i (meta word))) (+ 2 (* period 80))])

(defmulti arrow-joints
  "Where the arrows points w1->w2"
  (fn [w1 w2 w1-coords w2-coords]
    [(local-type w1) (local-type w2)])
  :hierarchy #'h)

(defmethod arrow-joints [::chunks ::expansion']
  [w1 w2 [x1 y1] [x2 y2]]
  (let [x' (- x1 1/2 (/ (rem (::sha1/t (meta w1)) 16) 20))
        input-pos (as-> w2 <>
                        (meta <>)
                        (get-in <> [:bitalgs/provenance :inputs])
                        (remove number? <>)
                        (map wordid <>)
                        (.indexOf <> (wordid w1)))
        y' (- y2 0.3 (* 0.1 input-pos))
        x'' (+ x2 -0.15 (* 0.1 input-pos))]
    [[x' y1] [x' y'] [x'' y'] [x'' y2]]))

(defmethod arrow-joints [::input' ::A]
  [w1 w2 [x1 y1] [x2 y2]]
  [[x1 y2]])

(defmethod arrow-joints [::f-result ::A]
  [w1 w2 [x1 y1] [x2 y2]]
  (let [x' (+ 1.5 x2)]
    [[x' y1] [x' y2]]))

(defmethod arrow-joints [::K ::A]
  [w1 w2 [x1 y1] [x2 y2]]
  (let [x' (dec x1)
        x'' (+ 0.1 x2)
        y' (- y2 0.35)]
    [[x' y1]
     [x' y']
     [x'' y']
     [x'' y2]]))

(defmethod arrow-joints [::A' ::A]
  [w1 w2 [x1 y1] [x2 y2]]
  (let [x' (- x1 0.1)]
    [[x' y1] [x' y2]]))

(defmethod arrow-joints [::A-sup ::f3]
  [w1 w2 [x1 y1] [x2 y2]]
  (let [y' (+ y1 2)
        x' (inc x1)]
    [[x1 y']
     [x' y']
     [x' y2]]))

(defmethod arrow-joints [::A-sup ::f1]
  [w1 w2 [x1 y1] [x2 y2]]
  (let [y' (+ y1 2)
        x' (inc x1)]
    [[x1 y']
     [x' y']
     [x' y2]]))

(defmethod arrow-joints [::A-sup ::C]
  [w1 w2 [x1 y1] [x2 y2]]
  (let [y' (+ y1 2)
        x' (inc x1)
        y'' (- y2 1.8)
        x'' (+ 0.3 x')
        y''' (dec y2)]
    [[x1 y']
     [x' y']
     [x' y'']
     [x'' y'']
     [x'' y''']
     [x2 y''']]))

(defmethod arrow-joints [::init ::output]
  [w1 w2 [x1 y1] [x2 y2]]
  (let [i (* 0.05 (::sha1/i (meta w1)))

        y' (+ y1 0.3 i)
        x' (+ 7 i)
        y'' (+ (dec y2) i)]
    [[x1 y']
     [x' y']
     [x' y'']
     [x2 y'']]))

(defmethod arrow-joints [::f3 ::f2]
  [w1 w2 [x1 y1] [x2 y2]]
  [[x2 y1]])

(defmethod arrow-joints [::f1 ::f-result]
  [w1 w2 [x1 y1] [x2 y2]]
  (let [x' (- x2 0.05)
        y' (- y2 0.3)]
    [[x1 y']
     [x' y']
     [x' y2]]))

(defmethod arrow-joints [::f2 ::f-result]
  [w1 w2 [x1 y1] [x2 y2]]
  (let [x' (+ x2 0.05)]
    [[x' y1] [x' y2]]))

(defmethod arrow-joints :default
  [w1 w2 _ _]
  (prn "UNPROCESSED" (map (comp name local-type) [w1 w2]))
  [])

(defn prov-data->svg
  [words]
  (let [layout (into {} (for [w words] [(wordid w) (coords w (::sha1/t (meta w)))]))
        op-coords (fn [wordid]
                    (let [[x y] (layout wordid)]
                      [x (- y 0.4)]))
        xs (->> layout vals (map first))
        ys (->> layout vals (map second))
        [minx maxx] (apply (juxt min max) xs)
        [miny maxy] (apply (juxt min max) ys)
        width (+ 4 (- maxx minx))
        height (+ 4 (- maxy miny))
        els (apply merge-with into
                   (for [w words
                         :let [{id :bitalgs/id
                                {:keys [inputs op-name]} :bitalgs/provenance
                                type ::sha1/type
                                t ::sha1/t}
                               (meta w)

                               hex (s/upper-case (bytes->hex w))
                               [x y] (layout id)]]
                     {:arrows
                      (for [input inputs
                            :when (w32/word32? input)
                            :let [p1 (layout (wordid input))
                                  p2 (op-coords id)
                                  joints (arrow-joints input w p1 p2)]]
                        [:g.word-arrow
                         (svg/polyline (concat [p1] joints [p2]))])

                      :ops
                      (if inputs
                        (let [[x' y'] (op-coords id)]
                          [[:g.op
                            (svg/line x' y' x y)
                            (svg/circle x' y' 0.2)
                            (svg/text x' (+ y' 0.05) (op-label op-name (filter number? inputs)))]]))

                      :words
                      [[:g.word
                        [:title (str (name type) ":" t)]
                        (svg/rect (- x 0.4) (- y 0.1) 0.8 0.2 {:rx 0.05, :ry 0.05})
                        (svg/text x (+ y 0.05) hex)]]}))]
    (svg* [(- minx 2)
           (- miny 2)
           width
           height]
          1000
          (int (* 1000 (/ height width)))
          (list
           [:style (slurp "bitalgs.css")]
           (concat (:arrows els) (:ops els) (:words els))))))

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
