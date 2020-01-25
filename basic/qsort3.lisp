(defun qsort (place comp list)
  (if (null list)
    list
    (qsort2 place comp (car list) (cdr list) nil nil) ))

(defun qsort2 (place comp p list left right)
  (if (null list)
    (append (qsort place comp left) (cons p (qsort place comp right)))
    (if (funcall comp (funcall place (car list)) (funcall place p))
      (qsort2 place comp p (cdr list) (cons (car list) left) right)
      (qsort2 place comp p (cdr list) left (cons (car list) right)) )))

(qsort #'car #'< '((2 . 20) (1 . 10) (3 . 30) (5 . 50) (4 . 40)))