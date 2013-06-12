(ns bitalgs.svg.sha1.logic
  (:refer-clojure :exclude [==])
  (:require [bitalgs.sha1 :as sha1]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(def wordid (comp :bitalgs/id meta))

(defn layout
  [words]
  (let [vs (zipmap (map wordid words)
                   (repeatedly lvar))
        w->v (comp vs wordid)]
    (first
     (run 1 [q]
          (== q vs)
          (everyg
           (fn [[w v]]
             (let [inputs (->> w
                               meta
                               :bitalgs/provenance
                               :inputs
                               (remove number?))
                   wt (type w)
                   input-vs (map w->v inputs)
                   is? (partial isa?
                                bitalgs.sha1/type-hierarchy
                                wt)
                   when-is (fn [t g] (if (is? t) g s#))

                   first-parent-that's
                   (fn [t]
                     (->> inputs
                          (filter #(isa? bitalgs.sha1/type-hierarchy (type %) t))
                          (first)
                          (w->v)))

                   with-parent
                   (fn [t f]
                     (let [v (first-parent-that's t)]
                       (fresh [x y]
                              (== v [x y])
                              (f x y))))]
               (all
                (fresh [x y]
                       (== v [x y])
                       (fd/in x (fd/interval 1 10))
                       (fd/in y (fd/interval 1 1000))

                       (when-is ::sha1/init
                                (== y 1))

                       (when-is ::sha1/ABCDE
                                (all
                                 (== x
                                     ({::sha1/A 3
                                       ::sha1/B 4
                                       ::sha1/C 5
                                       ::sha1/D 6
                                       ::sha1/E 7} wt))
                                 (with-parent ::sha1/initABCDE
                                   (fn [x' y']
                                     (fd/+ 6 y' y)))))

                       (when-is ::sha1/A
                                (with-parent ::sha1/input'
                                  (fn [x' y']
                                    (all
                                     (fd/+ 2 x' x)
                                     (fd/+ 1 y' y)))))

                       (when-is ::output
                                (with-parent ::ABCDE
                                  (fn [x' y']
                                    (fd/+ 2 y' y))))))))
           (map (juxt identity (comp vs wordid)) words))))))

(comment
(+ 1 2)

(def f (layout bitalgs.core/words))

(count (frequencies (vals f)))

(->> f
     (vals)
     (frequencies)
     (seq)
     (shuffle)
     (take 10))

(->> bitalgs.core/words
     (filter #(= [1 1] (f (wordid %))))
     (map type)
     (frequencies))
{:bitalgs.sha1/f2 40, :bitalgs.sha1/f1c 20, :bitalgs.sha1/A' 80, :bitalgs.sha1/output-A 1, :bitalgs.sha1/f3b 20, :bitalgs.sha1/f3a 20, :bitalgs.sha1/K 4, :bitalgs.sha1/init-E 1, :bitalgs.sha1/expansion' 64, :bitalgs.sha1/output-C 1, :bitalgs.sha1/f3c 20, :bitalgs.sha1/output-B 1, :bitalgs.sha1/init-B 1, :bitalgs.sha1/init-A 1, :bitalgs.sha1/output-D 1, :bitalgs.sha1/init-D 1, :bitalgs.sha1/init-C 1, :bitalgs.sha1/output-E 1, :bitalgs.sha1/f1 20, :bitalgs.sha1/f1b 20, :bitalgs.sha1/f1a 20, :bitalgs.sha1/f3 20}

((frequencies (vals f)) [1 1])
)
