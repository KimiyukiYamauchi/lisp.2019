(defun enq (e queue) (cons e queue))
(defun deq (queue) (reverse (cdr (reverse queue))))
(defun head (queue) (car (reverse queue)))