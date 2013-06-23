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

(defn derives
  "Like derive, but for deriving multiple children at once.
   Note the parent comes first."
  [h parent & children]
  (reduce
   #(derive %1 %2 parent)
   h
   children))
