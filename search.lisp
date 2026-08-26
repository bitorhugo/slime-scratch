;;
;; The idea behind bfs is that of searching for a goal
;;  within a network by maintaining a queue of unexplored nodes.
;; Enqueuing deeper nodes at the end, ensures that we search the
;;  network a layer at a time.
;; This implementation is from Paul Graham, and represents a slight
;;  complication of this idea.
;; We don't want just to find the goal, but know what path lead to it.
;; So instead of mantaining a queue of nodes, we mantain a queue of paths.
;;

;; network : ((a b c) (b c) (c d) (d e a))
;;

(defun shortest-path (start end net)
  "Returns the shortest path from START to END within NET."
  (bfs end '((a)) net))

(defun new-paths (path node net)
  "Returns the next layer of NET to explore."
  (let ((adjacent (cdr (assoc node net))))
    (mapcar #'(lambda (n)
		(cons n path))
	    adjacent)))

(defun bfs (end queue net)
  "Returns the path to END within NET."
  (when queue
    (let* ((path (first queue))
	   (node (first path)))
      (if (eql node end)
	  (reverse path)
	  (bfs end
	       (append (cdr queue)
		       (new-paths path node net))
	       net)))))

;;
;; This is a variation of the above, now tracking explored nodes
;;  using hash-tables and proper FIIFO struct for O(V + E)
;;

(defstruct queue
  (items nil)
  (tail nil))

(defun enqueue (q items)
  (dolist (item items)
    (let ((cell (list item)))
      (if (queue-tail q)
	  (setf (cdr (queue-tail q)) cell)
	  (setf (queue-items q) cell))
      (setf (queue-tail q) cell))))

(defun dequeue (q)
  (prog1
      (pop (queue-items q))
    (when (null (queue-items q))
      (setf (queue-tail q) nil))))

(defun queue-empty-p (q)
  (null (queue-items q)))

(defun new-paths (node path network visited)
  (let ((neighbors (remove-if (lambda (n)
				(gethash n visited))
			      (gethash node network))))
    (mapcar (lambda (n) (cons n path)) neighbors)))

(defun mark (paths visited)
  (mapc (lambda (path)
	  (let ((node (first path)))
	    (setf (gethash node visited) node)))
	paths)
  (values visited))

(defun %bfs (goal q network visited)
  (unless (queue-empty-p q)
    (let* ((path (dequeue q))
	   (node (first path)))
      (if (funcall goal node)
	  (reverse path)
	  (let ((paths (new-paths node path network visited)))
	    (enqueue q paths)
	    (mark paths visited)
	    (%bfs goal q network visited))))))

(defun bfs (goal start network)
  (let* ((q (make-queue))
	 (visited (make-hash-table)))
    (enqueue q (list (list start)))
    (setf (gethash start visited) start)
    (%bfs goal q network visited)))

(defun shortest-path (start end network)
  (bfs (lambda (node) (eql node end))
       start
       network))

#+n(let ((net (make-hash-table)))
  (setf (gethash 'a net) '(b c)
	(gethash 'b net) '(c)
	(gethash 'c net) '(d)
	(gethash 'd net) '(e a))
  (shortest-path 'b 'a net))

;;
;; Iterative version
;;

(defun reconstruct-path (parent start)
  (if (eql start (cdar parent))
      (list (caar parent) (cdar parent))
      (cons (caar parent)
	    (reconstruct (rest parent)
			 start))))

(defun bfs (goal start net)
  (let ((queue (list start))
	(visited (list start))
	(parent (list)))
    (do ((n (pop queue) (pop queue)))
	((null  n))
      (let ((value (funcall goal n visited parent)))
	(if value
	    (return-from bfs value)
	    (let ((neighbors (remove-if (lambda (nei)
					  (member nei visited))
					(cdr (assoc n net)))))
	      (dolist (neighbor neighbors)
		(push (cons neighbor n) parent)
		(push neighbor visited)
		(push neighbor queue))))))))

(defun shortest-path (start end net)
  (bfs (lambda (node visited parent)
	 (declare (ignore visited))
	 (when (eql node end)
	   (reconstruct-path parent start)))
       start net))
