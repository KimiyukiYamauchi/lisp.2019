- (3 + 2) * (4 + 5) / (2 + 3) => 9
(/ (* (+ 3 2) (+ 4 5)) (+ 2 3))
1. 36 + 48
2. 24 / 36
3. 24.0 36.0

- 3の2乗 => 9
(expt 3 2)
4. 3の100乗

- 3の平方根 => 1.7320508...
(sqrt 3)
5. 2の平方根
6. -9の平方根

(setf people '((RIE 19) (TOM 22) (KEVIN 37) (ERIKA 9)))
- リストpeopleの最初のアトム(要素)取得する(car/first)
  - (car people), (first people)
- リストpeopleの２番目以降のリストを取得する(cdr/rest)
- (second people) => (car (cdr people))
7. (third people)と同様処理をcar/cdrまたはfirst/restで行う
8. (fourth people)と同様処理をcar/cdrまたはfirst/restで行う
- 条件に合うアトムをリストから作事する(remove)
- peolpeリストから年齢が9のアトムを削除する
  - (remove 9 people :key #'second)
- peopleリストから年齢が20以上のアトムを削除する
  - (remove 20 people :test #'<= :key #'second)
- (setf people '((RIE 19 female) (TOM 22 male) (KEVIN 37 male) (ERIKA 9 female)))
9. pepleリストから男性(male)のアトムを削除する
