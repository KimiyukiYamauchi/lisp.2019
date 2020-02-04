(defun equal ( x y)
  (cond ((atom x) (eq x y))
        ((atom y) nil)  ; ここに来たときはxはアトムじゃない
        ((equal (car x) (car y))
          (equal (cdr x) (cdr y)) )))