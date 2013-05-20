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
    :bit-rotate-left (format "\u27f2%d" (first numeric-args))))

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
                                   (grouped category))})})))

(def period 6)
(def line-sep 0.1)

(defn f-box-dims
  [t]
  [2.5 (+ 1.25 (* t period)) 3 3])

(defn f-box-input-anchors
  [t i]
  [(+ 3 i) (+ 1.25 (* t period))])

(defn f-box-output-anchor
  [t]
  [4 (+ 4.25 (* t period))])

(defmulti coords (fn [x t] (type x))
  :hierarchy #'sha1/type-hierarchy)

(defmethods coords [word t]

  ::sha1/f
  [4 (+ 4 (* period t))]

  ::sha1/f1a
  [4 (+ 3 (* period t))]

  ::sha1/f1b
  [5 (+ 3 (* period t))]

  ::sha1/f1c
  [3 (+ 2 (* period t))]

  ::sha1/f3a
  [3 (+ 3 (* period t))]

  ::sha1/f3b
  [4 (+ 3 (* period t))]

  ::sha1/f3c
  [5 (+ 3 (* period t))]

  ::sha1/input'
  [0 (+ 5 (* period t))]

  ::sha1/expansion'
  [0 (+ 4 (* period t))]

  ::sha1/A
  [2 (* period (inc t))]

  ::sha1/A'
  [2 (- (* period (inc t)) 2)]

  ::sha1/B
  [3 (* period (inc t))]

  ::sha1/C
  [4 (* period (inc t))]

  ::sha1/D
  [5 (* period (inc t))]

  ::sha1/E
  [6 (* period (inc t))]

  ::sha1/K
  [8 (- (* period 20 (::sha1/i (meta word))) 0)]

  ::sha1/init
  [(+ 2 (::sha1/i (meta word))) 0]

  ::sha1/output
  [(+ 2 (::sha1/i (meta word))) (+ 2 (* period 80))])

(defmulti arrow-joints
  "Where the arrows points w1->w2"
  (fn [w1 w2 w1-coords w2-coords]
    [(type w1) (type w2)])
  :hierarchy #'sha1/type-hierarchy)

(defn rectilinear
  "Given a start point, an end point, an initial orientation (:horiz or :vert),
   and a sequence of coordinates which represent the constant
   coordinate for all the non-initial and non-final line segments,
   returns the points (including start and end) corresponding to a
   rectilinear polyline where the line segments have the coordinates
   given."
  [[x1 y1 :as start] [x2 y2 :as end] orient & coords]
  (if-let [[c & more] (seq coords)]
    (cons start
          (apply rectilinear
                 (case orient
                   :horiz [c y1]
                   :vert  [x1 c])
                 end
               ({:horiz :vert, :vert :horiz} orient)
                 more))
    [start (case orient :horiz [x2 y1] :vert [x1 y2]) end]))

(defn renaming-line
  [start end y1 i]
  (let [sep (* i line-sep)]
    (rectilinear start end :vert
                 (+ y1 0.6 sep)
                 (+ 6.5 (- sep))
                 (+ y1 period -0.75 (- sep)))))

(defmethods arrow-joints [w1 w2 [x1 y1 :as start] [x2 y2 :as end]]


  [::sha1/chunks ::sha1/expansion']
  (let [x' (- x1 1/2 (/ (rem (::sha1/t (meta w1)) 16) 20))
        input-pos (as-> w2 <>
                        (meta <>)
                        (get-in <> [:bitalgs/provenance :inputs])
                        (remove number? <>)
                        (map wordid <>)
                        (.indexOf <> (wordid w1)))
        y' (- y2 0.3 (* 0.1 input-pos))
        x'' (+ x2 -0.15 (* 0.1 input-pos))]
    (rectilinear start end :horiz x' y' x''))


  [::sha1/input' ::sha1/A]
  (rectilinear start end :vert)


  [::sha1/f ::sha1/A]
  (rectilinear start end :vert (- y2 (* 10 line-sep)))


  [::sha1/K ::sha1/A]
  (rectilinear start end :vert (- y2 (* 8 line-sep)) (+ 0.25 x2))


  [::sha1/A' ::sha1/A]
  (rectilinear start end :horiz (- x1 0.1))


  [::sha1/E-sup ::sha1/A]
  (rectilinear start end :vert (- y2 (* 9 line-sep)) (+ x2 line-sep))


  [::sha1/A-sup ::sha1/f1c]
  (rectilinear start end :vert
               (+ y1 2)
               (inc x1))


  [::sha1/A-sup ::sha1/f1a]
  (rectilinear start end :vert
               (+ y1 2)
               (inc x1))

  ;;
  ;; The four lines that are mostly renamings
  ;;

  [::sha1/A-sup ::sha1/B]
  (renaming-line start end y1 3)
  [::sha1/B-sup ::sha1/C]
  (renaming-line start end y1 2)
  [::sha1/C-sup ::sha1/D]
  (renaming-line start end y1 1)
  [::sha1/D-sup ::sha1/E]
  (renaming-line start end y1 0)


  ;;
  ;; f-stuff
  ;;

  [::sha1/B-sup ::sha1/f1a]
  (rectilinear start end :vert
               (- y2 1.25)
               (- x2 0.5))

  [::sha1/C-sup ::sha1/f1a]
  (rectilinear start end :vert)

  [::sha1/f1c ::sha1/f1b]
  (rectilinear start end :horiz (- x2 0.5))

  [::sha1/f1a ::sha1/f1]
  (rectilinear start end :vert)

  [::sha1/f1b ::sha1/f1]
  (rectilinear start end :vert)


  [::sha1/f1a ::sha1/f]
  (rectilinear start end :vert
               (- y2 0.3)
               (- x2 0.05))


  [::sha1/f1b ::sha1/f]
  (rectilinear start end :horiz (+ x2 0.05))

  [::sha1/B ::sha1/f2]
  (rectilinear start end :vert)

  [::sha1/C ::sha1/f2]
  (rectilinear start end :vert)

  [::sha1/D ::sha1/f2]
  (rectilinear start end :vert)

  [::sha1/f3a ::sha1/f3]
  (rectilinear start end :vert)

  [::sha1/f3b ::sha1/f3]
  (rectilinear start end :vert)

  [::sha1/f3c ::sha1/f3]
  (rectilinear start end :vert)

  [::sha1/B ::sha1/f3a]
  (rectilinear start end :vert)
  [::sha1/B ::sha1/f3b]
  (rectilinear start end :vert
               (- y2 1/2)
               (- x2 (/ line-sep 2)))
  [::sha1/C ::sha1/f3a]
  (rectilinear start end :vert
               (- y2 1)
               (+ x2 1/2))
  [::sha1/C ::sha1/f3c]
  (rectilinear start end :vert
               (- y2 1)
               (- x2 1/2))
  [::sha1/D ::sha1/f3b]
  (rectilinear start end :vert
               (- y2 1/2)
               (+ x2 (/ line-sep 2)))
  [::sha1/D ::sha1/f3c]
  (rectilinear start end :vert)

  [::sha1/init ::sha1/output]
  (let [i (* 0.05 (::sha1/i (meta w1)))]
    (rectilinear start end :vert
                 (+ y1 0.3 i)
                 (+ 7 i)
                 (+ (dec y2) i)
                 (+ 0.5 x2)))

)

(defmethod arrow-joints :default
  [w1 w2 _ _]
  (prn "UNPROCESSED" (map (comp symbol name type) [w1 w2]))
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
                                type :type
                                t ::sha1/t}
                               (meta w)

                               hex (s/upper-case (bytes->hex w))
                               [x y] (layout id)]]
                     {:arrows
                      (for [input inputs
                            :when (w32/word32? input)
                            :let [rename? (= :rename op-name)
                                  p1 (layout (wordid input))
                                  p2 (if rename? [x y] (op-coords id))
                                  joints (arrow-joints input w p1 p2)]]
                        [:g.word-arrow
                         (svg/polyline (concat [p1] joints [p2]))])

                      :ops
                      (if (and inputs (not= op-name :rename))
                        (let [[x' y'] (op-coords id)]
                          [[:g.op
                            (svg/line x' y' x y)
                            (svg/circle x' y' 0.2)
                            (svg/text x' (+ y' 0.07) (op-label op-name (filter number? inputs)))]]))

                      :words
                      [[:g.word
                        [:title (str (name type) ":" t)]
                        (svg/rect (- x 0.4) (- y 0.1) 0.8 0.2 {:rx 0.05, :ry 0.05})
                        (svg/text x (+ y 0.05) hex)]]}))
        f-boxes (for [i (range 80)]
                  [:g.f-box
                   (apply svg/rect (f-box-dims i))])]
    (svg* [(- minx 2)
           (- miny 2)
           width
           height]
          1000
          (int (* 1000 (/ height width)))
          (list
           [:style (slurp "bitalgs.css")]
           (concat f-boxes (:arrows els) (:ops els) (:words els))))))

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
