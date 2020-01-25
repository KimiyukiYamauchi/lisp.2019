; 全数検索(2階関数版)
; fun - 検索用関数
; key - 検索ワード
; list - データ列
(defun my-find (fun key list)
  (if (null list)
    nil
    (append
      (if (funcall fun key (car list))
        (list (car list)) )
      (my-find fun key (cdr list)) )))