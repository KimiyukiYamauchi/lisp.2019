;; (defparameter *width* 100) ; マップの横幅
;; (defparameter *height* 30)    ; マップの縦幅
;; (defparameter *jungle* '(45 10 10 10))  ; ジャングルの位置と縦と横の幅
(defparameter *width* 50) ; マップの横幅
(defparameter *height* 15)    ; マップの縦幅
(defparameter *jungle* '(30 5 5 5))  ; ジャングルの位置と縦と横の幅
(defparameter *plant-energy* 80)  ; 植物の持つ生命力、動物がこれを食べると80日生きられる

;; 植物の座標を保存するハッシュテーブル
(defparameter *plants* (make-hash-table :test #'equal))

;; 左端の位置(left, top)と幅((width)、高さ(height))を
;; 引数にとり、植物のランダムな座標を1つ追加する
(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height) ))))
    (setf (gethash pos *plants*) t)))

(defun add-plants ()
  (apply #'random-plant *jungle*) ; ジャングルへの植物の追加
  (random-plant 0 0 *width* *height*)) ; マップ全体への植物の追加

;; 動物のデータ構造
(defstruct animal x y energy dir genes)

;; 初期値は動物を１つだけ追加
(defparameter *animals*
  (list (make-animal  :x      (ash *width*  -1)
                      :y      (ash *height* -1)
                      :energy 1000
                      :dir    0
                      :genes  (loop repeat 8
                                collecting (1+ (random 10)) ))))

;; 動物の移動
(defun move (animal)
  (let  ((dir (animal-dir animal))  ; 現在の向き
          (x (animal-x animal))     ; 現在のx座標
          (y (animal-y animal)))    ; 現在のy座標

    ;; 移動する方向
    ;; 0 1 2
    ;; 7 * 3
    ;; 6 5 4
    (setf (animal-x animal)
          ;; 右方向は+1、左方向は-1、上下は0
          (mod (+ x
                  (cond ((and (>= dir 2) (< dir 5)) 1)
                        ((or (= dir 1) (= dir 5)) 0)
                        (t -1) ))
                *width*))
    (setf (animal-y animal)
          ;; 上方向は-1、舌方向は+1、左右は0
          (mod (+ y
                  (cond ((and (>= dir 0) (< dir 3)) -1)
                        ((and (>= dir 4) (< dir 7)) 1)
                        (t 0) ))
                *height*))
          ;; 1回の移動で-1減る
    (decf (animal-energy animal)) ))

;; 動物の方向
(defun turn (animal)
        ;; xに遺伝子情報の全ての数値を足した値の範囲から
        ;; ランダムな値が設定される
  (let ((x (random (apply #'+ (animal-genes animal)))))
    ;; ランダムな値xと遺伝子情報(genes)から方向が決まる
    (labels ((angle (genes x)
              (let ((xnu (- x (car genes))))
                (if (< xnu 0)
                    0
                    (1+ (angle (cdr genes) xnu)) ))))
      ;; 決まった方向をセットするS
      (setf (animal-dir animal)
            (mod (+ (animal-dir animal) (angle (animal-genes animal) x)) 8) ))))

;; 動物が草を食べる
(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*)
      (incf (animal-energy animal) *plant-energy*)
      (remhash pos *plants*) )))

(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
        ;; 現在の生命力をeにセット
  (let  ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      ;; 現在の生命力の半分を新しい生命力としてセット
      (setf (animal-energy animal) (ash e -1))
      (let  ((animal-nu (copy-structure animal))
            (genes      (copy-list (animal-genes animal)))
            (mutation   (random 8)))
        (setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1)))
        (setf (animal-genes animal-nu) genes)
        (push animal-nu *animals*) ))))

;;; 士ミューレションの世界の1日
(defun update-world()
  (setf *animals*
        ;; energyが0になっているanimalはリストから除く
        (remove-if 
          (lambda (animal)
            (<= (animal-energy animal) 0))
          *animals*))
  
  ;; それぞれの動物の活動
  (mapc
    (lambda (animal)
      (turn animal) ; 方向を変える
      (move animal) ; 移動する
      (eat animal)  ; 食べる
      (reproduce animal)) ; 増やす
    *animals*)
    
  ;; 植物を追加する処理
  (add-plants))

;;; 世界を描く
(defun draw-world ()
  (loop for y 
    below *height*  ; マップの高さ分の繰り返し
    do (progn  
              ;; マップの1行の描画処理
              (fresh-line)
              (princ "|") ; 左端の縦棒
              (loop for x
                    below *width*
                    do (princ (cond
                                ; その位置に動物がいればMを表示
                                ((some (lambda (animal)
                                          (and  (= (animal-x animal) x)
                                                (= (animal-y animal) y)))
                                        *animals*)
                                    #\M)
                                ; その位置に植物があれは#を表示
                                ((gethash (cons x y) *plants*) #\*)
                                ; その他はスペース表示
                                (t #\space))))
              (princ "|") ))) ; 右端の縦棒

;;; ユーザインタフェース
(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond 
      ((equal str "quit") ())
      (t  (let ((x (parse-integer str :junk-allowed t)))
            (if x
              (loop for i
                below x
                do (update-world)
                if (zerop (mod i 1000))
                  do (princ #\.) )
              (update-world))
            (evolution) )))))