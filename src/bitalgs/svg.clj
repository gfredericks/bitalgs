(ns bitalgs.svg
  "The generic code for drawing manually laid-out SVGs of
   algorithms."
  (:require [bitalgs.data :refer [bytes->hex]]
            [bitalgs.data.word32 :as w32]
            [clojure.string :as s]
            [com.gfredericks.svg-wrangler :refer [svg*] :as svg]
            [hiccup.core :refer [html]]))

(def op-sep
  "The distance between the center of a word and the center of its
   operator."
  0.4)

(def wordid (comp :bitalgs/id meta))

(defn op-label
  [kw args]
  (case kw
    :+ "+"
    :bit-or "\u2228"
    :bit-xor "\u2295"
    :bit-and "\u2227"
    :bit-not "!"
    :bit-rotate-left (str "\u27f2" (first args))))

(defn word
  [x y color text]
  [:g.word
   (svg/rect (- x 0.4) (- y 0.1) 0.8 0.2 {:rx 0.05,
                                          :ry 0.05,
                                          :fill color})
   (svg/text x (+ y 0.05) text)])

(defn op
  [x y op-name op-inputs]
  (list
   (svg/circle x y 0.2)
   (svg/text x (+ y 0.07) (op-label op-name op-inputs))))

(defn svg
  [position-fn arrow-joints-fn word-color-fn extra-stuff words]
  (let [layout (into {} (for [w words] [(wordid w) (position-fn w)]))
        op-coords (fn [wordid]
                    (let [[x y] (layout wordid)]
                      [x (- y op-sep)]))
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
                                type :type}
                               (meta w)

                               hex (s/upper-case (bytes->hex w))
                               [x y] (layout id)]]
                     {:arrows
                      (for [input inputs
                            :when (w32/word32? input)
                            :let [rename? (= :rename op-name)
                                  p1 (layout (wordid input))
                                  p2 (if rename? [x y] (op-coords id))
                                  joints (arrow-joints-fn input w p1 p2)]]
                        [:g.word-arrow
                         (svg/polyline (concat [p1] joints [p2]))])

                      :ops
                      (if (and inputs (not= op-name :rename))
                        (let [[x' y'] (op-coords id)]
                          [[:g.op
                            (svg/line x' y' x y)
                            (op x' y' op-name (filter number? inputs))]]))

                      :words
                      [(word x y (word-color-fn w) hex)]}))]
    (svg* [(- minx 2)
           (- miny 2)
           width
           height]
          1000
          (int (* 1000 (/ height width)))
          (list
           [:style (slurp "bitalgs.css")]
           extra-stuff
           (concat (:arrows els) (:ops els) (:words els))))))
