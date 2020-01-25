(defun enq (e queue)
  (if (null (car queue))
      (cons (cons e (car queue)) (cdr queue))
      (cons (car queue) (cons e (cdr queue))) ))

(defun deq (queue)
  (if (null (cdr (car queue)))
      (cons (reverse (cdr queue)) nil)
      (cons (cdr (car queue)) (cdr queue)) ))

(defun head (queue)
  (car (car queue)))