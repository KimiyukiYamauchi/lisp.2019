# Lisp関数

## 階乗計算
``` lisp
(defun fact (n)
  (if (<= n 1)
    1
    (* n (fact (- n 1))) ))
```

## 関数定義
### (defun 関数名 (引数 ...) 関数本体 ...)
``` lisp
(defun add1 (n) (+ n 1))
```
- add1を引数がnの1個で、n + 1を返す関数として定義

## 条件分岐if
### (if 条件節 then節 else節)
``` lisp
(if (> 5 3) "5 > 3 is true" "5 > 3 is false")
```
- -> "5 > 3 is true"

## リスト

### リストの先頭の要素を取り出す

#### (car リスト)
``` lisp
(car '(1 2 3))
```
- -> 1
``` lisp
(car ())
```
- -> NIL

### リストの先頭以外の残りのリストを取り出す

#### (cdr リスト)
``` lisp
(cdr '(1 2 3))
```
- -> (2 3)

### 要素をリストの先頭に加えたリストを生成する
#### (cons 要素 リスト)
``` lisp
(cons 1 '(2 3))
```
- -> (1 2 3)
``` lisp
(cons (car list) (cdr list))
```
- -> list
``` lisp
(cons 3 nil)
```
- -> (3)  
nilは空リスト
``` lisp
(cons 1 2)
```
- -> (1 . 2)  
リストの末尾がnilでないものはドット対

### リストを連結する
#### (append リスト ...)

``` lisp
(append '(1 2 3) '(4 5 6))
```
- -> (1 2 3 4 5 6)
``` lisp
(append '(1 2 3) '(4 5 6) '(7 8 9)) -> (1 2 3 4 5 6 7 8 9)
```
- -> (1 2 3 4 5 6 7 8 9)

### 引数のリストを生成する
#### (list 引数 ...)

``` lisp
(list 1 2 3)
```
- -> (1 2 3)  
(cons 1 (cons 2 (cons 3 nil)))と同じ
``` lisp
(list '(1 2) `(3 4))
```
- -> ((1 2) (3 4))

### リストの長さを返す
#### (length リスト)
``` lisp
(length '(1 2 3))
```
- -> 3
``` lisp
(length nil)
```
- -> 0

### 要素がリスト中にあるかどうかを返す
#### (member 要素 リスト)
``` lisp
(member 2 '(1 2 3))
```
- -> (2 3)
``` lisp
(member 4 '(1 2))
```
- -> nil  
nilは偽も表す

### 連想リスト((key1 . value1) (key2 . value2)...)からのキーと値の探索
#### (assoc key alist)
``` lisp
(assoc 2 '((1 . 10) (2 . 20) (3 . 30))) -> (2 .20)
(assoc 4 '((1 . 10) (2 . 20) (3 . 30))) -> NIL
```

### 連結リストの連結
#### (acons キー 値 連想リスト) -> (cons (cons key value) alist)

## シンボル

### シンボルの名前を取り出す関数
#### (symbol-name シンボル)
``` lisp
(symbol-name 'abc) -> "ABC"
```

### シンボルの値を取り出す関数
#### (symbol-value シンボル)
``` lisp
(setf abc 123) (symbol-value 'abc) -> 123
```

### シンボルの関数を取り出す関数
#### (symbol-function シンボル)
``` lisp
(defun abc () 123) (symbol-function 'abc) -> #<FUNCTION ...>
```

### シンボルの属性を取り出す
#### (symbol-plist シンボル)
``` lisp
(setf (get 'abc 'key) 'value) (symbol-plist 'abc) -> (KEY VALUE)
(setf (get 'abc 'key2) 'value2) (symbol-plist 'abc) -> (KEY2 VALUE2 KEY VALUE
```

### シンボルの属性リストからキーで値を取り出す関数
#### (get シンボル キー)
``` lisp
(setf (get 'abc 'key3) 'value3) (get 'abc 'key3) -> VALUE3
```

## 真理値と術後関数

### 値が同じか(アドレスが同じか)どうかを判断する関数
#### (eq 引数1 引数2)
``` lisp
(eq 1 1) -> T
(eq 'abc 'abc) -> T
(eq '(1 2 3) '(1 2 3)) -> NIL
```

### 内容が同じかどうかを判断する関数
#### (equal 引数1 引数2)
``` lisp
(equal '(1 2 3) '(1 2 3)) -> T
```

### リストが空であるかどうかを判断する関数
#### (null 引数)
``` lisp
(null '(1 2 3)) -> NIL
(null ()) -> T
(null 'abc) -> NIL
```

### 値がアトムであるかどうかを判断する関数
#### (atom 引数)
``` lisp
(atom 'abc) -> T
(atom '(1 2 3)) -> NIL
(atom ()) -> T
```

### 値が数型であるかどうかを判断する関数
#### (numberp 引数)
``` lisp
(numberp 123) -> T
(numberp 1.0) -> T
(numberp 2/3) -> T
```

## 関数型プログラミング向きの関数

### ラムダ式の定義
#### (lambda (引数...) ラムダ本体...)
``` lisp
(lambda (x) (+ x 1)) -> #<FUNCTION ...>
((lambda (x) (+ x 1) 10)) -> 11
(apply (lambda (x y) (cons x y)) '(10 20)) 
(funcall (funcall (lambda (x) (lambda (y) (+ x y))) 1) 2) -> 3
```

### 局所変数束縛
#### (let ((変数名 初期値) ...) 本体 ...)
``` lisp
(let ((x 1) (y 2)) (+ x y)) -> 3
(let (x y) (list x y)) -> (NIL NIL)
```

### 関数呼び出し
#### (funcall 関数 引数 ...)
``` lisp
(funcall #'+ 1 2 3) -> 6
(funcall (lambda (x y) (list x y)) 1 2) -> (1 2)
```

### 関数適用
#### (apply 関数 引数リスト ...)
``` lisp
(apply #'+ '(1 2 3)) -> 6
(apply #'list '(1 2 3) '(4 5 6) '(7 8 9)) 
```

### 関数評価
#### (eval 引数)
``` lisp
(eval '(+ 1 2)) -> 3
```

### マップ関数
#### (mapcar 関数 引数リスト ...)
``` lisp
(mapcar #'list '(1 2 3) '(4 5 6))
```
- その他のマップ関数
  - map
  - mapc
  - mapcon
  - mapl
  - maplist

  ### 型変換
  #### (coerce オブジェクト 変換する型)
  ``` lisp
  (coerce "THIS IS" 'list) => (#\T #\H #\I #\S #\Space #\I #\S)
  (coerce '(#\T #\H #\I #\S #\Space #\I #\S) 'string) => "THIS IS"
  ```

  #### (print1-to-string リスト)
  ``` lisp
  (print1-to-string ('1 2 3)) => "(1 2 3)"
  ```

  #### (string-trim 削除する文字列 文字列)
  ``` lisp
  (string-trim "(1 2 3)") => "1 2 3"
  ```

  #### (substitute-if 置き換える文字 関数 オブジェクト)
  ``` lisp
  (substitute-if #\e #'digit-char-p "I'm a l33t hack3r!) => "I'm a leet hacker!"
  (substitute-if 0 #'oddp '(1 2 3 4 5 6 7 8)) => (0 2 0 4 0 6 0 8)
  ```