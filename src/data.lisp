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

(defun hash-edges (edge-list)
)

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