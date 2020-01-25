; 全数検索(1階関数版)
; key - 検索ワード
; list - データ列
(defun my-find1 (key list)
    (if (null list)
      nil
      (append
        (if (eq key (car list)) ; eqで比較
          (list (car list)) )
        (my-find1 key (cdr list)))))