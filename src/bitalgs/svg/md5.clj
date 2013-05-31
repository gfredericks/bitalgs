(ns bitalgs.svg.md5
  (:require [bitalgs.md5 :as md5]
            [bitalgs.svg :as bsvg]
            [bitalgs.util :refer [defmethods]]
            [com.gfredericks.svg-wrangler :as svg]))

;;
;; Have to decide how to handle the little-endianness. Probably just
;; do the thing that makes the hex string come out the right way and
;; make a note about little-endianness.
;;

(def wordid (comp :bitalgs/id meta))

(def period 7)
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
                 [dispatch `(let [~'t (::md5/t (meta ~'word))
                                  ~'tp (if ~'t (* period ~'t))]
                              ~body)])
               (partition 2 dispatch+bodies))))

(coords-fns

  ::md5/FGHI
  [4 (+ 4 tp)]

  ::md5/Fa
  [4 (+ 3 tp)]

  ::md5/Fb
  [5 (+ 3 tp)]

  ::md5/Fc
  [3 (+ 2 tp)]

  ::md5/Ga
  [3 (+ 3 tp)]

  ::md5/Gb
  [4 (+ 3 tp)]

  ::md5/Gc
  [5 (+ 2 tp)]

  ::md5/Ia
  [3 (+ 3 tp)]

  ::md5/Ib
  [5 (+ 2 tp)]

  ::md5/A
  [2 (* period (inc t))]

  ::md5/B
  [3 (* period (inc t))]

  ::md5/Ba
  [3 (- (* period (inc t)) 1)]

  ::md5/Bb
  [3 (- (* period (inc t)) 2)]

  ::md5/C
  [4 (* period (inc t))]

  ::md5/D
  [5 (* period (inc t))]

  ::md5/T
  [7 (- tp 3)]

  ::md5/input
  [0 (+ tp 4)]

  ::md5/init
  [(+ 2 (::md5/i (meta word))) 0]

  ::md5/output
  [(+ 2 (::md5/i (meta word))) (+ 2 (* period 64))])

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

(defmacro defarrow-joints
  [& dispatch+exprs]
  `(defmethods arrow-joints [~'w1 ~'w2
                             [~'x1 ~'y1 :as ~'start]
                             [~'x2 ~'y2 :as ~'end]]
     ~@(mapcat
        (fn [[disp expr]]
          [disp `(let [~'recti (partial rectilinear ~'start ~'end)
                       ~'vrecti (partial ~'recti :vert)
                       ~'hrecti (partial ~'recti :horiz)]
                   ~expr)])
        (partition 2 dispatch+exprs))))

(defarrow-joints

  [::md5/input ::md5/Bb]
  (let [t (::md5/t (meta w1))]
    (hrecti (+ x1 1.4 (- (* t 0.06)))))

  [::md5/f ::md5/A]
  (vrecti (- y2 (* 10 line-sep)))


  [::md5/K ::md5/A]
  (vrecti (- y2 (* 8 line-sep)) (+ 0.25 x2))


  [::md5/A' ::md5/A]
  (hrecti (- x1 0.1))


  [::md5/E-sup ::md5/A]
  (vrecti (- y2 (* 9 line-sep)) (+ x2 line-sep))


  ;;
  ;; The four lines that are mostly renamings
  ;;

  [::md5/B-super ::md5/B]
  (vrecti (+ y1 1) (+ x1 2.7))

  [::md5/B-super ::md5/C]
  (vrecti (+ y1 1) (+ x1 2.7) (- y2 bsvg/op-sep))

  [::md5/C-super ::md5/D]
  (vrecti
   (+ y1 1 (- line-sep))
   (+ x1 1.7 line-sep)
   (- y2 bsvg/op-sep (- line-sep)))

  [::md5/D-super ::md5/A]
  (vrecti
   (+ y1 1 (* -2 line-sep))
   (- x2 line-sep)
   (- y2 1))

  ;;
  ;; The boxes
  ;;

  [::md5/Fc ::md5/Fb] (hrecti (- x2 0.5))
  [::md5/Fb ::md5/F] (vrecti)
  [::md5/B-super ::md5/Fa] (vrecti (- y2 1.25)
                                   (- x2 0.5))

  [::md5/Gc ::md5/Gb] (vrecti)
  [::md5/Ga ::md5/G] (vrecti)
  [::md5/D-super ::md5/Ga] (vrecti (- y2 1.25) (+ x2 0.5))

  [::md5/ABCD-super ::md5/H] (vrecti)

  [::md5/Ib ::md5/Ia] (vrecti)
  [::md5/Ia ::md5/I] (vrecti)

  ;;
  ;; Other stuff
  ;;

  [::md5/init ::md5/output]
  (let [i (* 0.05 (::md5/i (meta w1)))]
    (vrecti (+ y1 0.5 (- i))
            (+ 6 i)
            (+ (dec y2) i)
            (+ 0.5 x2)))

  [::md5/A-super ::md5/Bb]
  (vrecti
   (- y2 line-sep))

  [::md5/FGHI ::md5/Bb]
  (vrecti
   (- y2 line-sep))

  [::md5/T ::md5/Bb]
  (vrecti))

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
  [:g.reference {:transform "translate(-1.5, -1.75)"}
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
   (svg/text -1.5 3.5 "Input:" {:class "okay"})
   (svg/text -0.3 3.5 (pr-str s) {:class "then"})])

(defn svg
  [input-string words]
  (bsvg/svg
   coords
   arrow-joints
   fill-color
   (list (input-note input-string)
         (for [i (range 64)]
           [:g.f-box
            (apply svg/rect (f-box-dims i))])
         reference)
   words))

(defonce loaded (atom 0)) (swap! loaded inc)