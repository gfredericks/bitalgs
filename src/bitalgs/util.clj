(ns bitalgs.util)

(defmacro defmethods
  "Helper for doing lots of defmethods. Assumes each
   body is a single expressiond and all definitions have
   the same arglist."
  [mname arglist & dispatches+bodies]
  (cons 'do
        (for [[dispatch body] (partition 2 dispatches+bodies)]
          `(defmethod ~mname ~dispatch
             ~arglist
             ~body))))

(def wordid (comp :bitalgs/id meta))

(defn word-inputs
  [word]
  (->> word
       meta
       :bitalgs/provenance
       :inputs
       (remove number?)))
