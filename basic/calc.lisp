(+ 1 2 3 4 5)
(- 10 25)
(* 3 7)
(/ 63 9)
(- (* 6 5) 10)
(+ 1 (+ 2 (+ 3 (+ 4 5))))
(+ (+ (+ (+ 1 2) 3) 4 ) 5)
; setf, car, cdr, null, atom
(setf A nil)
(setf B '(87 58 90))
(setf C '(I B M))
(null t) => nil
(null A) => T
(null (null A)) => T
(cadr B) => 58
; (car (cdr B))
(+ (car B) (caddr B)) => 177
; (caddr B) = (car (cdr (cdr B)))
(cadr C) => B
; (car (cdr C))
(setf (car C) (cdr B))
=> C => ((58 90) B M)
; (car C) => I
; (cdr B) => (58 90)
(atom C) => nil
(atom (car (car C))) => T
(atom (cdddr B)) => T
; (cdddr B) => (car (cdr (cdr (cdr B))))
(setf (car C) (cdr (car C)))
=> ((90) B M)
(null (cdr (car C))) => T
(atom (caar B)) => error
; (caar B) => (car (car B))
(cons 'A nil) => (A)
(list nil) => (NIL)
(list 'A nil) => (A NIL)
; 間違いやすいので注意!
(cons (cons 'A nil) (cons 'B nil))
=> ((A) B)
(list (list 1) (list 2) (list 3))
=> ((1) (2) (3))
(cons 'head (list 'body 'tail))
=> (HEAD BODY TAIL)
; (A B), (C D) => (A B C D)
(cons '(A B) '(C D))  ; 間違い例
(setf x (list 'stones 'scissors 'papers))
(setf y x)
(setf z (list 'stones 'scissors 'papers))
(eq nil nil) => T
(eq '() nil) => T
(eq 'A t) => nil
(eq 1115 1115) => T
(eq 201 (+ 190 11)) => T
(eq 1 'one) => nil
(eq x '(stones scissors papers)) => nil
(equal x '(stones scissors papers)) => T
(eq y x) => T
(eq x z) => nil
(equal x z) => T
(eq (cdr x) (cdr y)) => T
(eq (cdr x) (cdr z)) => nil
(eq 3 '(+ 1 2)) => nil
(eq (car x) (car z)) => T
(eq (cadr y) 'scissors) => T
(eq (cdddr x) (cdddr z)) => T
(defun !third (list)
  (car (cdr (cdr list))) )
(defun !fourth (list)
  (car (cdr (cdr (cdr list)))) )
; 引数のlistの最後の要素を返す関数
(defun last-elem (list)
  (cond ((null list) nil)
        (t (last-elem2 list)) ))

(defun last-elem2 (list)
  (cond ((null (cdr list)) (car list))
        (t (last-elem2 (cdr list))) ))

; 引数のlistの要素の個数を返す関数
(defun !length (list)
  (cond ((null list) 0)
        ((null (cdr list)) 1)
        (t (1+ (!length2 (cdr list)))) ))

(defun !length2 (list)
  (cond ((null (cdr list)) 1)
        (t (1+ (!length2 (cdr list)))) ))

(defun random-animal()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

(loop repeat 10
  do (format t "~5t~a ~15t~a ~25t~a~%"
        (random-animal)
        (random-animal)
        (random-animal)))

(loop repeat 10
  do (format t "~30<~a~;~a~;~a~>~%"
        (random-animal)
        (random-animal)
        (random-animal)))

(loop repeat 10
  do (format t "~30:@<~a~;~a~;~a~>~%"
        (random-animal)
        (random-animal)
        (random-animal)))

(loop repeat 10
  do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
        (random-animal)
        (random-animal)
        (random-animal)))

(defparameter *animal* (loop repeat 10 collect (random-animal)))

(format t "~{I see a ~a! ~}" *animal*)

(format t "~{I see ~a... or was it a~a?~%~}" *animal*)

(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))

