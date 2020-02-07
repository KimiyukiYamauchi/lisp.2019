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

(defun last-elem (list)
  (do ((x (setq x list)))
      ((null (cdr x)) (car x)) 
      (setq x (cdr x)) ))

(defun last-elem (list)
  (do ((x (setq x list) (setq x (cdr x))))  ; 変数制限、初期化、ループの頭に戻ってきたら実行する処理
      ((null (cdr x)) (car x)) )) ; 終了条件、戻り値

; 引数のlistの要素の個数を返す関数
(defun !length (list)
  (cond ((null list) 0)
        ((null (cdr list)) 1)
        (t (1+ (!length2 (cdr list)))) ))

(defun !length2 (list)
  (cond ((null (cdr list)) 1)
        (t (1+ (!length2 (cdr list)))) ))

(defun !length (x)
  (do ((count 0 (1+ count)))
      ((null x) count)
      (setq x (cdr x)) ))

; リストのn番目の要素を返す関数
(defun !nth (n x)
  (cond ((< n 0) (error 'illegal-argument 'nth  n x))
        (t (!nth2 n x)) ))

(defun !nth2 (n x)
  (cond
  ; xがnilだったらnil
        ((null x) nil)
  ; xがnilだったらerror
        ;((null x) (error 'overrun 'nth n x))
  ; nが1だったら(car x)
        ((= n 0) (car x))
  ; nが2以上だったら再帰呼び出し
    ; (1- n)
    ; (cdr x)
        (t (!nth2 (1- n) (cdr x))) ))

(defun !nth2 (n x)
  (do ( (i (setq i 0) (setq i (1+ i)))
        (tmp (setq tmp x)) )
      ((= i n) (car tmp))
      (setq tmp (cdr tmp)) ))

; ２つのリストをつなぐ関数
(defun !append (a b)
  (cond 
        ((null a) b)
        (t (cons (car a) (!append (cdr a) b))) ))

(defun !append (a b)
  (do ((count (setq count (1- (!length a))) (setq count (1- count)))
      (tmp (setq tmp b)))
      ((= count 0) (cons (car a) tmp))
      (setq tmp (cons (!nth count a) tmp)) ))


(cons 
  (car '(east south)) 
  (cons (cadr '(east south)) '(west north)) )

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

(defun robots ()
  (loop named main
        with directions = '((q . -65) (w . -64) (e . -63) (a . -1)
                            (d . 1)   (z . 63)  (x .64)   (c 65))
        for pos = 544
        then (progn (format t "~%qwe/asd/zxc to movve, (t)eleport, (l)eave:")
                    (force-output)
                    (let* ( (c (read))
                            (d (assoc c (directions))))
                      (cond (d (+ pos (cdr d)))
                            ((eq 't c)  (random 1024))
                            ((eq 'l c)  (return-form main 'bye))
                            (t pos))))
        for monsters = (loop repeat 10
                          collect (random 1024))
        then (loop for mpos in monsters
                collect (if (> (count mpos monsters) 1)
                            mpos
                            (cdar (sort (loop for (k .d) in directions
                                              for new-mpos = (+ mpos d)
                                              collect (cons (+  (abs (- (mod new-mpos 64)
                                                                        (mod pos 64)))
                                                                (abs (- (ash new-mpos -6)
                                                                        (ash pos -6))))
                                                            new-mpos))
                                        '<
                                        :key #'car))))
        when (loop for mpos in monsters
                )))