(defun find-blank (board)
  (position-if #'zerop board))

(defun finished-p (board)
  (and (zerop (aref board 0))
       (dotimes (i 7 t)
	 (when (< (aref board (1+ i))
		  (aref board i))
	   (return-from finished-p nil)))))


(defun valid-moves (board)
  (let ((current (find-blank board)))
    (remove-if (lambda (dir)
                 (or (and (eql dir :up)    (< current 3))
                     (and (eql dir :down)  (> current 5))
                     (and (eql dir :left)  (zerop (mod current 3)))
                     (and (eql dir :right) (zerop (mod (1+ current) 3)))))
               (list :up :down :left :right))))


(defun swap-positions (board x y)
  (let ((tmp (aref board x)))
    (setf (aref board x) (aref board y))
    (setf (aref board y) tmp)))

(defun make-move (board move)
  (let ((current (find-blank board)))
    (swap-positions board
		    (ecase move
		      (:up (- current 3))
		      (:down (+ current 3))
		      (:left (1- current))
		      (:right (1+ current)))
		    current))
  (values board))


(defun print-board (board)
  (dotimes (i 3)
    (dotimes (j 3)
      (format t "| ~a " (aref board (+ (* i 3) j))))
    (format t "|~%")))


(defun move (board)
  (let ((valid (valid-moves board)))
    (format t "~%Board:~%")
    (print-board board)
    (format t "Valid Moves >> ~{~a~^ ~}~%" valid)
    (restart-case
        (let ((choice (intern (string-upcase (symbol-name (read))) :keyword)))
          (assert (member choice valid))
          (make-move board choice))
      (redo-move ()
        :report "Try again"
        (move board)))))

(defun play ()
  (let ((board (make-array 9 :initial-contents '(4 1 6 5 2 8 7 3 0))))
    (loop until (finished-p board)
	  do (move board))))
