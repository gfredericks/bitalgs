(ns bitalgs.core
  (:require [bitalgs.data :refer [bytes->hex]]
            [bitalgs.graphviz :as gv]
            [bitalgs.sha1 :as sha1]
            [bitalgs.data.word32 :as w32]
            [bitalgs.svg :as bsvg]
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

(def period 6)
(def line-sep 0.1)

(defn f-box-dims
  [t]
  [2.5 (+ 1.25 (* t period)) 3 3])

(defmulti coords type
  :hierarchy #'sha1/type-hierarchy)

(defmacro coords-fns
  [& dispatch+bodies]
  `(defmethods coords [~'word]
     ~@(mapcat (fn [[dispatch body]]
                 [dispatch `(let [~'t (::sha1/t (meta ~'word))] ~body)])
               (partition 2 dispatch+bodies))))

(coords-fns

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
                 (+ y1 0.5 (- i))
                 (+ 7 i)
                 (+ (dec y2) i)
                 (+ 0.5 x2)))

)

(defmethod arrow-joints :default
  [w1 w2 _ _]
  (prn "UNPROCESSED" (map (comp symbol name type) [w1 w2]))
  [])

(defmulti fill-color type :hierarchy #'sha1/type-hierarchy)

(defmethods fill-color [w]
  ;; Lucas recommended making the colors brighter.
  ::sha1/constant "#ffdfe5" #_"#ffc0cb"
  ::sha1/input "#d4f6d4" #_"#AAEEAA"
  ::sha1/output "#99bbf6" #_"#3377EE"
  :default "#e5e5e5" #_"#CCCCCC")


(def reference
  [:g.reference {:transform "translate(-1.5, -1)"}
   (svg/rect 0 0 2.6 4.75 {:class "border"})
   (svg/text 0.1 0.4
             "SHA1"
             {:font-size 0.4
              :fill "black"
              :font-family "Helvetica"})
   (for [[type txt i]
         [[::sha1/input "Input (with 0's and length)" 0]
          [::sha1/constant "Algorithm constant" 1]
          [::sha1/output "Output" 2]]
         :let [dy (+ 0.75 (* i 0.35))]]
     [:g {:transform (format "translate(0,%f)" dy)}
      (bsvg/word 0.5 0 (fill-color (with-meta {} {:type type})) "0123abcd")
      (svg/text 1 0.03 txt {:class "item"})])
   (for [[op-name op-inputs txt i]
         [[:+ [] "32-bit addition" 0]
          [:bit-or [] "Bitwise OR" 1]
          [:bit-xor [] "Bitwise XOR" 2]
          [:bit-and [] "Bitwise AND" 3]
          [:bit-not [] "Bitwise NOT" 4]
          [:bit-rotate-left ["n"] "Bit-rotate left by n" 5]]
         :let [dy (+ 1.9 (* i 0.5))]]
     [:g {:transform (format "translate(0,%f)" dy)}
      [:g.op (bsvg/op 0.3 0 op-name op-inputs)]
      (svg/text 1 0.03 txt {:class "item"})])])

(defn input-note
  [s]
  [:g.input
   (svg/text -1.5 4.25 "Input:" {:class "okay"})
   (svg/text -0.3 4.25 (pr-str s) {:class "then"})])

(defn sha1-svg
  [input-string words]
  (bsvg/svg
   coords
   arrow-joints
   fill-color
   (list (input-note input-string)
         (for [i (range 80)]
           [:g.f-box
            (apply svg/rect (f-box-dims i))])
         reference)
   words))

(let [input-string "denny"
      words (->> (.getBytes input-string)
                 (seq)
                 (sha1/sha1-words)
                 (provenance-data))]
  (spit "sha1.svg"
        (html (sha1-svg input-string words))))

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
