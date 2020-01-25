(defun last-element (x)
  (cond ((null x) nil)
        (t (last-element2 x)) ))

(defun last-element2 (x)
  (cond  ((null (cdr x) (car x))
          (t (last-element2 (cdr x)))) ))