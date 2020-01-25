(defun nqueen (n)
  (nqueen2 n 1 nil) )

; 引数n --- 女王の人数(盤の大きさ)
; y --- 配置しようとする女王の縦位置
; board --- 盤(縦位置(y座標)を要素とするx座標のリスト(x座標は降順))
; 戻り地 --- y以上n以下のy座標に配置できる全パターンをリストにして返す
(defun nqueen2 (n y board)
  (if (> y n)
    nil
    ; (member y board)は横報告に他の女王がいるかどうか
    ; (diagonal 1 y board)は斜め報告に他の女王がいるかどうか
    ; 縦方向は女王を1個ずつしか配置しないことでチェック不要
    (if (or (member y board) (diagonal 1 y board))
      (nqueen2 n (+ y 1) board) ; 他の女王がいたときは次のy座標にする
      (append ; 以下の2つのリストを連結
        ; yの位置で配置できるパターン
        (if (= (length board) (- n 1))
          (list (cons y board)) ; 最後の女王が配置できたとき
          (nqueen2 n 1 (cons y board)) )  ; 次の女王を配置する
        ; y+1からnの位置で配置できるパターン
        (nqueen2 n (+ y 1) board) ))))

; queenの位置に女王がおけるかどうか
; 駒が置けなければTを返す
; boardの長さ分のチェックをする
(defun diagonal (x queen board)
  (if (null board)
    nil
    (if (= (abs (- (car board) queen)) x) ; 斜めのチェック
      t ; absは絶対値を取る関数
      (diagonal (+ x 1) queen (cdr board)) )))