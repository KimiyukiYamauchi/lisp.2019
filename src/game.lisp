(defparameter *nodes* 
'((living-room (you are in the living-room.))
(garden (you are in a beautiful garden. there is a well in front of you.))
(attic (you are int the attic.
there is a giant welding torch in the corner.))))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(defparameter *edges*
'((living-room (garden west door)
                (attic upstairs ladder))
    (garden (living-room east door))
    (attic (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from hear.))