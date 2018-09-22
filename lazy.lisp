(defmacro lazy (&body body)
  (let ((forced (gensym))
	(value (gensym)))
    `(let ((,forced nil)
	   (,value nil))
       (lambda ()
	 (unless ,forced
	   (setf ,value (progn ,@body))
	   (setf ,forced t))
	 ,value))))

(defun force (lazy-value)
  (funcall lazy-value))



(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))


;; A lazy list for holding all integer numbers

;; (defparameter *integers*
;;   (labels ((f (n)
;; 	     (lazy-cons n (f (1+ n)))))
;;     (f 1)))

;; (lazy-car *integers*)
;; (lazy-car (lazy-cdr *integers*))
;; (lazy-car (lazy-cdr (lazy-cdr *integers*)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defun make-lazy (list)
  (lazy (when list
	  (cons (car list)
		(make-lazy (cdr list))))))



(defun take (n list)
  (unless (or (zerop n)
	      (lazy-null list))
    (cons (lazy-car list)
	  (take (1- n)
		(lazy-cdr list)))))

(defun take-all (list)
  (unless (lazy-null list)
    (cons (lazy-car list)
	  (take-all (lazy-cdr list)))))



(defun lazy-mapcar (fun list)
  (lazy (unless (lazy-null list)
	  (cons (funcall fun (lazy-car list))
		(lazy-mapcar fun (lazy-cdr list))))))

(defun lazy-mapcan (fun list)
  (labels ((f (list-cur)
	     (if (lazy-null list-cur)
		 (force (lazy-mapcan fun (lazy-cdr list)))
		 (cons (lazy-car list-cur)
		       (lazy (f (lazy-cdr list-cur)))))))
    (lazy (unless (lazy-null list)
	    (f (funcall fun (lazy-car list)))))))

(defun lazy-find-if (fun list)
  (unless (lazy-null list)
    (let ((x (lazy-car list)))
      (if (funcall fun x)
	  x
	  (lazy-find-if fun (lazy-cdr list))))))

(defun lazy-nth (n list)
  (if (zerop n)
      (lazy-car list)
      (lazy-nth (1- n)
		(lazy-cdr list))))


;; Tests:
;; (take 10 (lazy-mapcar #'sqrt *integers*))
;; (take 10
;;       (lazy-mapcan (lambda (x)
;; 		     (if (evenp x)
;; 			 (make-lazy (list x))
;; 			 (lazy-nil)))
;; 		   *integers*))
;; (lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10)))
;; (lazy-nth 4 (make-lazy '(a b c d e f g)))


