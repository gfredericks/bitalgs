(ns bitalgs.svg.layout
  "Functions for describing the layout of a typed DAG."
  (:refer-clojure :exclude [==])
  (:require [bitalgs.data :as data]
            [clojure.core.logic :refer :all]
            [clojure.core.logic.fd :as fd]))

(defn set-X
  "Set the absolute X value for the given type."
  [type x]
  {:type type
   :goal (fn [v]
           (fresh [y] (== [x y] v)))})

(defn set-Y
  "Set the absolute Y value for the given type."
  [type y]
  {:type type
   :goal (fn [v]
           (fresh [x] (== [x y] v)))})

(defn set-X-diff
  "Set the relative X separation between the given input output
   pair of types."
  [input-type output-type x]
  {:types [input-type output-type]
   :goal (fn [in-v out-v]
           (matche [in-v out-v]
                   ([[x1 y1] [x2 y2]]
                      (fd/+ x1 x x2))))})

(defn set-Y-diff
  "Set the relative XY separation between the given input output
   pair of types."
  [input-type output-type y]
  {:types [input-type output-type]
   :goal (fn [in-v out-v]
           {:pre [in-v out-v]}
           (matche [in-v out-v]
                   ([[x1 y1] [x2 y2]]
                      (fd/+ y1 y y2))))})

(defn set-XY-diff
  "Set the relative X and Y separation between the given input output
   pair of types."
  [input-type output-type x y]
  {:types [input-type output-type]
   :goal (fn [in-v out-v]
           (matche [in-v out-v]
                  ([[x1 y1] [x2 y2]]
                     (fd/+ x1 x x2)
                     (fd/+ y1 y y2))))})

(defn set-Y-diff-min-per-input
  "Declares the minimum value for the Y-diff between two types, as
   well as the assertion that the given value actually exists -- i.e.,
   it is the infimum."
  [input-type output-type min-y]
  {:types [input-type output-type]
   :scope :input
   :goal (fn [in-v outputs]
           (let [diffs (repeatedly (count outputs) lvar)]
             (matche [in-v]
                     ([[_ y1]]
                        (everyg
                         (fn [[out-v diff]]
                           (matche [out-v]
                                   ([[_ y2]]
                                      (fd/+ y1 diff y2)
                                      (fd/<= min-y diff))))
                         (map list outputs diffs))
                        (membero min-y diffs)))))})

(defn ^:private set-rectangular-domain
  [vars [minx miny width height]]
  (everyg
   (fn [var]
     (fresh [x y]
            (== var [x y])
            (fd/in x (fd/interval minx (+ minx width)))
            (fd/in y (fd/interval miny (+ miny height)))))
   vars))

(defn solve-layout
  "Graph is a sequence of typed traceable values. Each constraint
   should be as returned from the functions above. All coordinates
   must be positive integers. Third arg is [minx miny width height].

   Returns a map from IDs to [x y]."
  [tvals hierarchy rectangle & constraints]
  {:post [(map? %)]}
  (let [isa? (partial isa? hierarchy)
        pairs (for [tval tvals, input (data/traceable-inputs tval)] [input tval])
        ids (map data/id tvals)
        vars (zipmap ids (repeatedly lvar))

        ;; Using memoize for premature optimization. Should actually
        ;; test if it helps?
        constraints-for-single-type
        (memoize
         (fn [type]
           (filter
            (fn [{type' :type}]
              (and type' (isa? type type')))
            constraints)))

        constraints-for-pair-of-types
        (memoize
         (fn [input-type output-type]
           (filter
            (fn [{[itype otype] :types}]
              (and itype
                   otype
                   (isa? input-type itype)
                   (isa? output-type otype)))
            constraints)))]
    (first
     (run 1 [q]
          (set-rectangular-domain (vals vars) rectangle)
          (== q vars)
          (everyg identity
                  (for [tval tvals
                        :let [var (vars (data/id tval))]
                        constraint (constraints-for-single-type (type tval))
                        :when (nil? (:scope constraint))]
                    ((:goal constraint) var)))
          (everyg identity
                  (for [[input-val output-val] pairs
                        :let [input-var (vars (data/id input-val))
                              output-var (vars (data/id output-val))
                              _ (if-not input-var
                                  (throw (ex-info "POOP" {:id (data/id input-val)
                                                          :keys (take 2 (keys vars))
                                                          :type (type input-val)
                                                          :val input-val})))]
                        constraint (constraints-for-pair-of-types
                                    (type input-val)
                                    (type output-val))
                        :when (nil? (:scope constraint))]
                    ((:goal constraint) input-var output-var)))
          (everyg identity
                  (for [{:keys [scope goal]
                         [itype otype] :types} constraints
                         :when (= :input scope)
                         :let [pairs
                               (for [[input-val output-val] pairs
                                     :when (isa? (type input-val) itype)
                                     :when (isa? (type output-val) otype)
                                     :let [input-var (vars (data/id input-val))
                                           output-var (vars (data/id output-val))]]
                                 [input-var output-var])
                               grouped-pairs (group-by first pairs)]
                         [input pairs] grouped-pairs
                         :let [outputs (map second pairs)]]
                    (goal input outputs)))))))
