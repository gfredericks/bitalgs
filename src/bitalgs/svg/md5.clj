(ns bitalgs.svg.md5
  (:require [bitalgs.md5 :as md5]
            [bitalgs.svg :as bsvg]
            [bitalgs.util :refer [defmethods]]
            [com.gfredericks.svg-wrangler :as svg]))

(def wordid (comp :bitalgs/id meta))

(def period 6)
(def line-sep 0.1)

(defn f-box-dims
  [t]
  [2.5 (+ 1.25 (* t period)) 3 3])

(defmulti coords type
  :hierarchy #'md5/type-hierarchy)

(defmacro coords-fns
  [& dispatch+bodies]
  `(defmethods coords [~'word]
     ~@(mapcat (fn [[dispatch body]]
                 [dispatch `(let [~'t (::md5/t (meta ~'word))] ~body)])
               (partition 2 dispatch+bodies))))

(coords-fns

  ::md5/f
  [4 (+ 4 (* period t))]

  ::md5/f1a
  [4 (+ 3 (* period t))]

  ::md5/f1b
  [5 (+ 3 (* period t))]

  ::md5/f1c
  [3 (+ 2 (* period t))]

  ::md5/f3a
  [3 (+ 3 (* period t))]

  ::md5/f3b
  [4 (+ 3 (* period t))]

  ::md5/f3c
  [5 (+ 3 (* period t))]

  ::md5/input'
  [0 (+ 5 (* period t))]

  ::md5/expansion'
  [0 (+ 4 (* period t))]

  ::md5/A
  [2 (* period (inc t))]

  ::md5/A'
  [2 (- (* period (inc t)) 2)]

  ::md5/B
  [3 (* period (inc t))]

  ::md5/C
  [4 (* period (inc t))]

  ::md5/D
  [5 (* period (inc t))]

  ::md5/E
  [6 (* period (inc t))]

  ::md5/K
  [8 (- (* period 20 (::md5/i (meta word))) 0)]

  ::md5/init
  [(+ 2 (::md5/i (meta word))) 0]

  ::md5/output
  [(+ 2 (::md5/i (meta word))) (+ 2 (* period 80))])

(defmulti arrow-joints
  "Where the arrows points w1->w2"
  (fn [w1 w2 w1-coords w2-coords]
    [(type w1) (type w2)])
  :hierarchy #'md5/type-hierarchy)

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


  [::md5/chunks ::md5/expansion']
  (let [x' (- x1 1/2 (/ (rem (::md5/t (meta w1)) 16) 20))
        input-pos (as-> w2 <>
                        (meta <>)
                        (get-in <> [:bitalgs/provenance :inputs])
                        (remove number? <>)
                        (map wordid <>)
                        (.indexOf <> (wordid w1)))
        y' (- y2 0.3 (* 0.1 input-pos))
        x'' (+ x2 -0.15 (* 0.1 input-pos))]
    (rectilinear start end :horiz x' y' x''))


  [::md5/input' ::md5/A]
  (rectilinear start end :vert)


  [::md5/f ::md5/A]
  (rectilinear start end :vert (- y2 (* 10 line-sep)))


  [::md5/K ::md5/A]
  (rectilinear start end :vert (- y2 (* 8 line-sep)) (+ 0.25 x2))


  [::md5/A' ::md5/A]
  (rectilinear start end :horiz (- x1 0.1))


  [::md5/E-sup ::md5/A]
  (rectilinear start end :vert (- y2 (* 9 line-sep)) (+ x2 line-sep))


  [::md5/A-sup ::md5/f1c]
  (rectilinear start end :vert
               (+ y1 2)
               (inc x1))


  [::md5/A-sup ::md5/f1a]
  (rectilinear start end :vert
               (+ y1 2)
               (inc x1))

  ;;
  ;; The four lines that are mostly renamings
  ;;

  [::md5/A-sup ::md5/B]
  (renaming-line start end y1 3)
  [::md5/B-sup ::md5/C]
  (renaming-line start end y1 2)
  [::md5/C-sup ::md5/D]
  (renaming-line start end y1 1)
  [::md5/D-sup ::md5/E]
  (renaming-line start end y1 0)


  ;;
  ;; f-stuff
  ;;

  [::md5/B-sup ::md5/f1a]
  (rectilinear start end :vert
               (- y2 1.25)
               (- x2 0.5))

  [::md5/C-sup ::md5/f1a]
  (rectilinear start end :vert)

  [::md5/f1c ::md5/f1b]
  (rectilinear start end :horiz (- x2 0.5))

  [::md5/f1a ::md5/f1]
  (rectilinear start end :vert)

  [::md5/f1b ::md5/f1]
  (rectilinear start end :vert)


  [::md5/f1a ::md5/f]
  (rectilinear start end :vert
               (- y2 0.3)
               (- x2 0.05))


  [::md5/f1b ::md5/f]
  (rectilinear start end :horiz (+ x2 0.05))

  [::md5/B ::md5/f2]
  (rectilinear start end :vert)

  [::md5/C ::md5/f2]
  (rectilinear start end :vert)

  [::md5/D ::md5/f2]
  (rectilinear start end :vert)

  [::md5/f3a ::md5/f3]
  (rectilinear start end :vert)

  [::md5/f3b ::md5/f3]
  (rectilinear start end :vert)

  [::md5/f3c ::md5/f3]
  (rectilinear start end :vert)

  [::md5/B ::md5/f3a]
  (rectilinear start end :vert)
  [::md5/B ::md5/f3b]
  (rectilinear start end :vert
               (- y2 1/2)
               (- x2 (/ line-sep 2)))
  [::md5/C ::md5/f3a]
  (rectilinear start end :vert
               (- y2 1)
               (+ x2 1/2))
  [::md5/C ::md5/f3c]
  (rectilinear start end :vert
               (- y2 1)
               (- x2 1/2))
  [::md5/D ::md5/f3b]
  (rectilinear start end :vert
               (- y2 1/2)
               (+ x2 (/ line-sep 2)))
  [::md5/D ::md5/f3c]
  (rectilinear start end :vert)

  [::md5/init ::md5/output]
  (let [i (* 0.05 (::md5/i (meta w1)))]
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

(defmulti fill-color type :hierarchy #'md5/type-hierarchy)

(defmethods fill-color [w]
  ;; Lucas recommended making the colors brighter.
  ::md5/constant "#ffdfe5" #_"#ffc0cb"
  ::md5/input "#d4f6d4" #_"#AAEEAA"
  ::md5/output "#99bbf6" #_"#3377EE"
  :default "#e5e5e5" #_"#CCCCCC")


(def reference
  [:g.reference {:transform "translate(-1.5, -1)"}
   (svg/rect 0 0 2.6 4.75 {:class "border"})
   (svg/text 0.1 0.4
             "MD5"
             {:font-size 0.4
              :fill "black"
              :font-family "Helvetica"})
   (for [[type txt i]
         [[::md5/input "Input (with 0's and length)" 0]
          [::md5/constant "Algorithm constant" 1]
          [::md5/output "Output" 2]]
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

(defn svg
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
