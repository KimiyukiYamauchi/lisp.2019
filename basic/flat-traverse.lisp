(defun flat-traverse (tree fun)
  (reverse (flat-traverse2 tree fun nil)) )

(defun flat-traverse2 (tree fun ret)
  (if (null tree)
    ret
    (if (atom (car tree))
        (flat-traverse2 (cdr tree) fun (cons (funcall fun (car tree)) ret))
        (flat-traverse2 (cdr tree) fun (flat-traverse2 (car tree) fun ret)) )))

(flat-traverse '(1 (2 3) (4 5)) (lambda (x) (+ x 1)))

(setf document
      '("document"  ("chapter 1"
                      ("section 1" "sentence 1" "sentence 2")
                      ("section 2" "sentence 1" "sentence 2" "sentence 3"))
                    ("chapter 2"
                      ("section 1" "sentence 1" "sentence 2" "sentence 3" "sentence 4")
                      ("section 2" "sentence 1" "sentence 2" "sentence 3") )))

(flat-traverse document (lambda (x) (length x)))

(flat-traverse document (lambda (x) x))