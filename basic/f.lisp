(defun foo (x &optional (y 2) (z (+ x y))) (list x y z))

(foo 5 3 2)
(foo 5 3)
(foo 5)