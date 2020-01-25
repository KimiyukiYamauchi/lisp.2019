; クイックソート(2階関数版)
; fun 比較関数、list 入力データ
(defun qsort (fun list)
  (if (null list)
    list
    ; クイックソートのピボット(基準値)は戦闘の値(car list)にする
    (qsort2 fun (car list) (cdr list) nil nil) ))

; fun 比較関数、p ピボット、list データ、left 比較でtrue、right そうでないもの
(defun qsort2 (fun p list left right)
  (if (null list)
    (append (qsort fun left) (cons p (qsort fun right)))
    ; ピボットpよりもfunなものをleftにそうでないものをrightに入れる
    (if (funcall fun (car list) p)
      (qsort2 fun p (cdr list) (cons (car list) left) right)
      (qsort2 fun p (cdr list) left (cons (car list) right)) )))

(qsort  (lambda (x y) (< (car x) (car y)))
        '((2 . 20) (1 . 10) (3 . 30) (5 . 50) (4 . 40)))

(qsort  (lambda (x y) (< (mod (car x) 5) (mod (car y) 5)))
        '((2 . 20) (1 . 10) (3 . 30) (5 . 50) (4 . 40)))