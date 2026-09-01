;;; Birthday Problem
;;
;; A: Given a group of N people, how large does N need to be for at least
;; a 50% chance of at least 2 people sharing a birthday
;;


(let ((cache (make-hash-table)))
  
  (defun clr-cache () (clrhash cache))

  (defun memoized () (values cache))

  (defun memoize (k v)
    (setf (gethash k cache) v))

  (defun %factorial (n)
    (loop for i from 1 to n
	  with res = 1
	  do (setf res (* i res))
	     (memoize i res)
	  finally (return res)))
  
  (defun factorial (n)
    (%factorial n)))


(defun odds (n)
  "Probability of N persons to NOT share a birthday. "
  (loop for i from 1 to n
	collect (float (/ (factorial 365)
			  (* (factorial (- 365 i))
			     (expt 365 i))))))

;;; Plotting

(ql:quickload :cl-svg :silent t)

(defparameter *plot-width* 640)
(defparameter *plot-height* 400)
(defparameter *plot-margin* 40)

(defun plot-odds (n &optional (filename "birthday-odds.svg"))
  (let* ((values (odds n))
	 (inner-w (- *plot-width* (* 2 *plot-margin*)))
	 (inner-h (- *plot-height* (* 2 *plot-margin*)))
	 (x0 *plot-margin*)
	 (y0 *plot-margin*)
	 (x->px (lambda (i) (+ x0 (* (/ (float i) (1- n)) inner-w))))
	 (y->px (lambda (p) (+ y0 (* (- 1 p) inner-h))))
	 (curve-points (loop for p in values
			     for i from 0
			     collect (list (funcall x->px i) (funcall y->px p)))))
    (svg:with-svg-to-file
	(scene 'svg:svg-1.1-toplevel :height *plot-height* :width *plot-width*)
	(filename :if-exists :supersede)
      ;; horizontal gridlines + y-axis labels at 0%, 25%, ..., 100%
      (dotimes (i 5)
	(let* ((p (/ i 4.0))
	       (y (funcall y->px p)))
	  (svg:draw scene (:line :x1 x0 :y1 y :x2 (+ x0 inner-w) :y2 y)
		    :stroke "lightgray" :stroke-width 1)
	  (svg:text scene (:x (- x0 8) :y (+ y 4) :font-size 10 :text-anchor "end")
	    (format nil "~D%" (round (* p 100))))))
      ;; the 50% reference line
      (let ((y-50 (funcall y->px 0.5)))
	(svg:draw scene (:line :x1 x0 :y1 y-50 :x2 (+ x0 inner-w) :y2 y-50)
		  :stroke "red" :stroke-dasharray "4,4"))
      ;; x-axis ticks + labels every 10 people
      (loop for i from 0 to (1- n) by 10
	    do (let ((x (funcall x->px i)))
		 (svg:draw scene (:line :x1 x :y1 (+ y0 inner-h) :x2 x :y2 (+ y0 inner-h 4))
			   :stroke "black")
		 (svg:text scene (:x x :y (+ y0 inner-h 16) :font-size 10 :text-anchor "middle")
		   (format nil "~D" (1+ i)))))
      ;; axes
      (svg:draw scene (:line :x1 x0 :y1 y0 :x2 x0 :y2 (+ y0 inner-h)) :stroke "black")
      (svg:draw scene (:line :x1 x0 :y1 (+ y0 inner-h) :x2 (+ x0 inner-w) :y2 (+ y0 inner-h))
		:stroke "black")
      ;; the odds curve
      (svg:draw scene (:polyline :points (svg:points curve-points))
		:fill "none" :stroke "steelblue" :stroke-width 2))
    filename))
