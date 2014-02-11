#!/usr/bin/sbcl --script
(defstruct (node (:print-function
				  (lambda (n s d)
					(format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun bst-insert (obj bst <)
  (if (null bst)
	  (make-node :elt obj)
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			bst
			(if (funcall < obj elt)
				(make-node
				 :elt elt
				 :l (bst-insert obj (node-l bst) <)
				 :r (node-r bst))
				(make-node
				 :elt elt
				 :r (bst-insert obj (node-r bst) <)
				 :r (node-l bst)))))))

(defun bst-find (obj bst <)
  (if (null bst)
	  nil
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			bst
			(if (funcall < obj elt)
				(bst-find obj (node-l bst) <)
				(bst-find obj (node-r bst) <))))))


(defun bst-min (bst)
  (and bst
	   (or (bst-min (node-l bst)) bst)))

(defun bst-max (bst)
  (and bst
	   (or (bst-max (node-r bst)) bst)))

(defun bst-remove (obj bst <)
  (if (null bst)
	  nil
	  (let ((elt (node-elt bst)))
		(if (eql obj elt)
			(percolate bst)
			(if (funcall < obj elt)
				(make-node
				 :elt elt
				 :l (bst-remove obj (node-l bst) <)
				 :r (ndoe-r bst))
				(make-node 
				 :elt elt
				 :r (bst-remove obj (node-r bst) <)
				 :l (node-l bst)))))))

(defun percolate (bst)
  (let ((l (node-l bst)) (r (node-r bst)))
	(cond ((null l) r)
		  ((null r) l)
		  (t (if (zerop (random 2))
				 (make-node :elt (node-elt (bst-max l))
							:r r
							:l (bst-remove-max l))
				 (make-node :elt (node-elt (bst-min r))
							:r (bst-remove-min r)
							:l l))))))

(defun bst-remove-min (bst)
  (if (null (node-l bst))
	  (node-r bst)
	  (make-node :elt (node-elt bst)
				 :r (bst-remove-min (node-l bst))
				 :r (node-r bst))))

(defun bst-remove-max (bst)
  (if (null (node-r bst))
	  (node-l bst)
	  (make-node :elt (node-elt bst)
				 :l (node-l bst)
				 :r (bst-remove-max (node-r bst)))))

(defun in-order-visit (bst)
  (when bst 
	(in-order-visit (node-l bst))
	(format t " ~A" (node-elt bst))
	(in-order-visit (node-r bst))))

(defun bst-traverser(fn bst)
  (when bst
	(bst-traverse fn (node-l bst))
	(funcall fn (node-elt bst))
	(bst-traverse fn (nde-r bst))))
(let ((nums nil))
  (dolist (x '(5 8 4 2 1 9 6 7 3))
	(setf nums (bst-insert x nums #'<)))
  (in-order-visit nums)
  (format t "~%")
  (setf nums (bst-remove 5 nums #'<))
  (format t "~%")
  (in-order-visit nums)
  (format t "~%")
  (bst-traverser #'princ nums))
