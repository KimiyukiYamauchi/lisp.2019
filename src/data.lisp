; リスト(イミュータブル) -> 配列(ミュータブル)
; 連想リスト(イミュータブル) -> ハッシュテーブル(ミュータブル)
; イミュータブル -> JavaのString型
; ミュータブル -> JavaのStringBuffer
(make-array 3)
(defparameter x (make-array 3))
(aref x 1)
(setf (aref x 1) 'foo)
(setf foo (list 'a 'b 'c))
(make-hash-table)
(defparameter x (make-hash-table))
(gethash 'yup x)
(setf (gethash 'yup x) '25)
(defparameter *drink-order* (make-hash-table))
(setf (gethash 'bill *drink-order*) 'double-espresso)
(setf (gethash 'lisa *drink-order*) 'small-drip-coffee)
(setf (gethash 'john *drink-order*) 'medium-latte)

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels ((traverse (node)
                (unless (member node visited)
                  (push node visited)
                  (mapc (lambda (edge)
                            (traverse (cdr edge)))
                          (direct-edges node edge-list)))))
        (traverse node))
    visited))

(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
            (let ((node (car x)))
              (push (cdr x) (gethash node tab))))
          edge-list)
    tab))

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
                (unless (gethash node visited)
                  (setf (gethash node visited) t)
                  (mapc (lambda (edge)
                            (traverse edge))
                          (gethash node edge-tab)))))
        (traverse node))
    visited))
(setf *edge-num* 1000)
(setf *node-num* 1000)
(time (dotimes (i 100) (get-connected 1 (make-edge-list))))
(time (dotimes (i 100) (get-connected-hash 1 (hash-edges (make-edge-list))))))

; 構造体
(defstruct person
        name
        age
        waist-size
        favorite-color)
(defparameter *bob* (make-person
                      :name "Bob"
                      :age 35
                      :waist-size 32
                      :favorite-color "blue"))
(person-age *bob*)
(setf (person-age *bob*) 36)
; データをジェネリックに扱う
(length '(a b c))
(length "abc")
(length (make-array 5))
; シーケンス関数
(find-if #'numberp '(a b 5 d))
(count #\s "mississippi")
(position #\4 "zkewl4skewl")
(some #'numberp '(a b 5 d))
(every #'numberp '(a b 5 d))
; シーケンスの要素について繰り返す関数
(reduce #'+ '(3 4 6 5 2))
(reduce (lambda (best item)
          (if (and (evenp item) (> item best))
            item
            best))
          '(7 4 6 5 2)
          :initial-value 0)
; バグっている処理
(reduce (lambda (best item)
          (if (and (evenp item) (> item best))
            item
            best))
          '(7 4 6 5 2))
(defun sum (lst)
  (reduce #'+ lst))
(sum '(1 2 3))
(sum (make-array 5 :initial-contents '(1 2 3 4 5)))
; 以下は動かない
(sum "12345")
(map 'list
      (lambda (x)
        (if (eq x #\s)
          #\S
          x))
      "this is a string")
(subseq "american" 2 6)
(sort '(5 8 2 4 9 3 6) #'<)
(numberp 5)
(defun add (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b ))
        ((and (listp a) (listp b)) (append a b))))
(defmethod add ((a number) (b number))
  (+ a b ))
(defmethod add ((a list) (b list))
  (append a b ))
(defmethod add ((a string) (b string))
  (concatenate 'string a b))