(defun traverse (tree fun)
  (if (null tree)
    nil
    (if (atom (car tree))
      (cons (funcall fun (car tree)) (traverse (cdr tree) fun))
      (cons (traverse (car tree) fun) (traverse (cdr tree) fun)) )))