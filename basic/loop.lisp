(loop repeat 5
      for x = 10.0
      then (/ x 2)
      collect x)

(loop for i
  below 4
  when (oddp i)
    do (print i)
    do (print "yup") )

(loop for i
  below 4
  when (oddp i)
    do (print i) )

(loop for i
  in '(0 2 4 6)
  always (evenp i) )