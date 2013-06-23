(ns bitalgs.svg.sha1
  (:require [bitalgs.data :as data]
            [bitalgs.sha1 :as sha1]
            [bitalgs.svg :as bsvg]
            [bitalgs.svg.layout :as layout]
            [bitalgs.util :refer [defmethods]]
            [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [com.gfredericks.svg-wrangler :as svg]))

(def period 6)
(def line-sep 0.1)

(defn f-box-dims
  [t]
  [2.5 (+ 1.25 (* t period)) 3 3])

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
                        (data/traceable-inputs <>)
                        (map data/id <>)
                        (.indexOf <> (data/id w1)))
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
  #_(prn "UNPROCESSED" (map (comp symbol name type) [w1 w2]))
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

(defn make-layout
  [words]
  (layout/solve-layout words sha1/type-hierarchy [0 0 10 1000]
                       (layout/set-Y ::sha1/init  0)
                       (layout/set-X ::sha1/A-all 2)
                       (layout/set-X ::sha1/B-all 3)
                       (layout/set-X ::sha1/C-all 4)
                       (layout/set-X ::sha1/D-all 5)
                       (layout/set-X ::sha1/E-all 6)
                       (layout/set-X ::sha1/K     8)

                       (layout/set-Y-diff  ::sha1/initABCDE  ::sha1/ABCDE      6)
                       (layout/set-Y-diff  ::sha1/ABCDE      ::sha1/output     2)
                       (layout/set-Y-diff  ::sha1/expansion' ::sha1/expansion  1)
                       (layout/set-XY-diff ::sha1/input'     ::sha1/A          2 1)
                       (layout/set-XY-diff ::sha1/A'         ::sha1/A          0 2)
                       (layout/set-XY-diff ::sha1/f          ::sha1/A         -2 2)
                       (layout/set-XY-diff ::sha1/f1a        ::sha1/f1         0 1)
                       (layout/set-XY-diff ::sha1/f1b        ::sha1/f1        -1 1)
                       (layout/set-XY-diff ::sha1/f1c        ::sha1/f1b        2 1)
                       (layout/set-XY-diff ::sha1/f3a        ::sha1/f3         1 1)
                       (layout/set-XY-diff ::sha1/f3b        ::sha1/f3         0 1)
                       (layout/set-XY-diff ::sha1/f3c        ::sha1/f3        -1 1)
                       (layout/set-Y-diff-min-per-input ::sha1/K ::sha1/A 6)))

(defn svg
  [input-string words]
  (bsvg/svg
   (comp (make-layout words) data/id)
   arrow-joints
   fill-color
   (list (input-note input-string)
         (for [i (range 80)]
           [:g.f-box
            (apply svg/rect (f-box-dims i))])
         reference)
   words))
