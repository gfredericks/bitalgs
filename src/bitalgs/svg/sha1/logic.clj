(ns bitalgs.svg.sha1.logic
  (:refer-clojure :exclude [==])
  (:require [bitalgs.data :as data]
            [bitalgs.sha1 :as sha1]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(defn prep-vars
  [words]
  (let [vs (zipmap (map data/id words)
                   (repeatedly lvar))

        things (for [w words]
                 {:word w
                  :type (type w)
                  :var (vs (data/id w))})

        pairs
        (for [w words
              input (data/traceable-inputs w)]
          {:from      (vs (data/id input))
           :from-type (type input)
           :to        (vs (data/id w))
           :to-type   (type w)})]
    {:vars vs
     :words things
     :pairs pairs}))

(defmacro wheng
  [test & gs]
  `(if ~test (all ~@gs) ~'s#))

(defne xy+
  [u v w]
  ([[x y] [x' y'] [x'' y'']]
     (fd/+ x x' x'')
     (fd/+ y y' y'')))

(defn layout
  "Returns a map from wordids to positions."
  [words]
  (let [{vars :vars, pairs :pairs, things :words} (prep-vars words)
        isa? (partial isa? sha1/type-hierarchy)]
    (first
     (run 1 [q]
          (== q vars)
          (everyg
           (fn [{:keys [word type var]}]
             (fresh [x y]
                    (== var [x y])
                    (fd/in x (fd/interval 0 10))
                    (fd/in y (fd/interval 0 1000))
                    (wheng (isa? type ::sha1/init) (== y 0))
                    (wheng (isa? type ::sha1/initABCDEoutput)
                           (== x
                               (case (last (name type))
                                 \A 2
                                 \B 3
                                 \C 4
                                 \D 5
                                 \E 6)))
                    (wheng (isa? type ::sha1/K)
                           (== x 8))))
           things)
          (everyg
           (fn [{:keys [from from-type to to-type]}]
             (let [when-types
                   (fn [from-type' to-type' g]
                     (wheng (and (isa? from-type from-type')
                                 (isa? to-type to-type'))
                            g))

                   xydelta
                   (fn [from-type to-type xy]
                     (when-types from-type to-type (xy+ xy from to)))]
               (all
                (fresh [x y x' y']
                       (== from [x' y'])
                       (== to [x y])

                       (when-types ::sha1/initABCDE ::sha1/ABCDE
                                   (fd/+ 6 y' y))

                       (when-types ::sha1/input' ::sha1/A
                                   (all
                                    (fd/+ 2 x' x)
                                    (fd/+ 1 y' y)))

                       (when-types ::sha1/ABCDE ::sha1/output
                                   (fd/+ 2 y' y))

                       (when-types ::sha1/expansion' ::sha1/expansion
                                   (fd/+ 1 y' y))

                       (xydelta ::sha1/A'  ::sha1/A   [0 2])
                       (xydelta ::sha1/f   ::sha1/A   [-2 2])
                       (xydelta ::sha1/f1a ::sha1/f1  [0 1])
                       (xydelta ::sha1/f1b ::sha1/f1  [-1 1])
                       (xydelta ::sha1/f1c ::sha1/f1b [2 1])
                       (xydelta ::sha1/f3a ::sha1/f3  [1 1])
                       (xydelta ::sha1/f3b ::sha1/f3  [0 1])
                       (xydelta ::sha1/f3c ::sha1/f3  [-1 1])

                       (when-types ::sha1/K ::sha1/A
                                   (fresh [d]
                                          (fd/+ d y' y)
                                          (fd/> d 5)
                                          (fd/< d 121)))))))
           pairs)))))
