(defun curry (fn &rest args)
  (lambda (&rest more-args)
    (apply fn (append args more-args))))

(defun bipartition (lst function)
  (loop for el in lst
	if (funcall function el)
	  collect el into true-bag
	else
	  collect el into false-bag
	finally (return (values true-bag false-bag))))

(defun quicksort (lst function)
  (when lst
    ;; choose pivot
    (destructuring-bind (p . rest) lst
      ;; partition lst against pivot
      (multiple-value-bind (left right)
	  (bipartition rest (curry function p))
	(append (quicksort left function) ; all elements to the left of p
		(list p)
		(quicksort right function)))))) ; all elements to the right of p
