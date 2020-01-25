(defun bank-deposit (money bank)
  (cons (+ money (car bank)) (cdr bank)) )

(defun bank-withdraw (money bank)
  (if (<= money (car bank)) 
    (cons money (cons (- (car bank) money) (cdr bank)))
    (cons 0 bank) ))