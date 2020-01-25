(defun deposit (money piggy-bank)
  (+ money piggy-bank) )

(defun withdraw (money piggy-bank)
  (if (<= money piggy-bank)
    (cons money (- piggy-bank money))
    (cons 0 piggy-bank) ))