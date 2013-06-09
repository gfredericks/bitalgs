(ns bitalgs.svg.sha1.logic
  (:refer-clojure :exclude [==])
  (:require [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(def wordid (comp :bitalgs/id meta))

(defn mino
  "w is (min x y)"
  [x y w]
  (conde
   [(== x y) (== x w)]
   [(fd/< x y) (== x w)]
   [(fd/> x y) (== y w)]))

(defn maxo
  "w is (min x y)"
  [x y w]
  (conde
   [(== x y) (== x w)]
   [(fd/< x y) (== y w)]
   [(fd/> x y) (== x w)]))

(defn zipgoal
  [g coll1 coll2]
  (matche [coll1 coll2]
          ([[] []])
          ([[x1 . xs1] [x2 . xs2]]
             (g x1 x2)
             (zipgoal g xs1 xs2))))
(defn xso
  [xys xs]
  (zipgoal firsto xys xs))

(defn yso
  [xys ys]
  (zipgoal #(fresh [x]
                   (== [x %2] %1))
           xys
           ys))

(defn layout
  [words]
  (let [vs (zipmap (map wordid words)
                   (repeatedly lvar))]
    (run 1 [q]
         (== q (vals vs))
         (everyg
          (fn [[w v]]
            (let [inputs (->> w
                              meta
                              :bitalgs/provenance
                              :inputs
                              (remove number?))
                  input-vs (map (comp vs wordid) inputs)]
              (if (seq inputs)
                (all
                 (fd/in x (fd/interval 0 500))))
              #_(if (seq inputs)
                (fresh [x y ys y']
                       (== v [x y])
                       (fd/in x (fd/interval 0 10))
                       (fd/in y (fd/interval 0 500))
                       #_(yso input-vs ys)
                       #_(fd/+ y' 1 y)
                       (everyg (fn [input-v]
                                 (fresh [vx vy]
                                        (== input-v [vx vy])
                                        (fd/> y vy)))
                               input-vs)
                       #_(membero y' ys))
                (fresh [x y]
                       (== v [x y])
                       (== y 0)))))
          (map (juxt identity (comp vs wordid)) words)))))

(comment
(+ 1 2)

(def f (future (first (layout bitalgs.core/words))))

(future-done? f)

(count @f)
  )
