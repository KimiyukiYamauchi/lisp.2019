(defun qsort (list)
 (if (null list)
    list
    (qsort2 (car list) (cdr list) nil nil) ))

(defun qsort2 (p list left right)
  (if (null list)
    (append (qsort left) (cons p (qsort right)))
    (if (< (car list) p)
      (qsort2 p (cdr list) (cons (car list) left) right)
      (qsort2 p (cdr list) left (cons (car list) right)) )))