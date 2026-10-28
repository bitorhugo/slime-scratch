;; leaf nodes (valence 0)
(defun fixed (value)
  `(:fixed ,value))

(defun empty-cell (domain)
  `(:empty ,domain))

;; structural nodes (valence 1+)
(defun region (name cells)
  `(:region ,name ,cells))

(defun all-different (regions)
  `(:all-different ,regions))

(defparameter *mini-sudoku*
  (all-different
   (list
    (region "Row 1" (list (fixed 4) (empty-cell '(1 2 3 4)) (empty-cell '(1 2 3 4)) (fixed 1)))
    (region "Row 2" (list (empty-cell '(1 2 3 4)) (fixed 3) (fixed 4) (empty-cell '(1 2 3 4)))))))

(defun fold-sudoku (ast &key on-fixed on-empty on-region on-all-different)
  "the unique homomorphism mapping the sudoku ast into any target algebra."
  (ecase (first ast)
    (:fixed
     (funcall on-fixed (second ast)))

    (:empty
     (funcall on-empty (second ast)))

    (:region
     (let ((name (second ast))
           (cells (third ast)))
       (funcall on-region name
                ;; Recursively fold every cell in the region
                (mapcar (lambda (c)
                          (fold-sudoku c :on-fixed on-fixed :on-empty on-empty
                                         :on-region on-region :on-all-different on-all-different))
                        cells))))

    (:all-different
     (let ((regions (second ast)))
       (funcall on-all-different
                ;; Recursively fold every region in the constraints
                (mapcar (lambda (r)
                          (fold-sudoku r :on-fixed on-fixed :on-empty on-empty
                                         :on-region on-region :on-all-different on-all-different))
                        regions))))))

(defun render-board (ast)
  (fold-sudoku ast
	       :on-fixed (lambda (value) (format nil " ~A " value))
	       :on-empty (lambda (domain)
			   (declare (ignore domain))
			   " . ")
	       :on-region (lambda (name rendered-cells)
			    (format nil "~A: [~{~A~^|~}]" name rendered-cells))
	       :on-all-different (lambda (rendered-regions)
				   (format nil "--- SUDOKU BOARD ---~%~{~A~%~}" rendered-regions))))

;; Executing it:
;; (format t (render-board *mini-sudoku*))
