(defun curry (function &rest args)
  (lambda (&args more-args)
    (apply function (append args more-args))))

(defun rcurry (function &rest args)
  (lambda (&args more-args)
    (apply function (append more-args args))))

