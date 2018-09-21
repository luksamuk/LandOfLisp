(load "macros.lisp") ;; pairs, etc

(defun print-tag (name alist closingp)
  (princ #\<)
  (when closingp (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
	  (format t " ~a=\"~a\""
		  (string-downcase (car att))
		  (cdr att)))
	alist)
  (princ #\>))


(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
		     (list ,@(mapcar (lambda (x)
				       `(cons ',(car x) ,(cdr x)))
				     (pairs atts)))
		     nil)
	  ,@body
	  (print-tag ',name nil t)))

;; macroexpand this to see effects (C-c M-m on selection)
;; (tag mytag (color 'blue height (+ 4 5)))

;; Uncomment and eval to test nested tags:

;; (tag mytag (color 'blue size 'big)
;;   (tag first_inner_tag ())
;;   (tag second_inner_tag ()))

;; html example
;; (tag html ()
;;   (tag body ()
;;     (princ "Hello world!")))

;; Now back to SVG at once

(defmacro svg (width height &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
		   "xmlns:xlink" "http://www.w3.org/1999/xlink"
		   height ,height
		   width ,width)
     ,@body))

(defun brightness (color amount)
  (mapcar (lambda (x)
	    (min 255 (max 0 (+ x amount))))
	  color))

;; eval to test
;;(brightness '(255 0 0) -100)

(defun svg-style (color)
  (format nil
	  "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
	  (append color
		  (brightness color -100))))

(defun circle (center radius color)
  (tag circle (cx (car center)
		  cy (cdr center)
		  r radius
		  style (svg-style color))))

;; testing
;; (svg 150 150
;;   (circle '(50 . 50) 50 '(255 0 0))
;;   (circle '(100 . 100) 50 '(0 0 255)))

(defun polygon (points color)
  (tag polygon (points (format nil "~{~a,~a ~}"
			       (mapcan (lambda (tp)
					 (list (car tp) (cdr tp)))
				       points))
		       style (svg-style color))))

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
	  (random-walk (if (zerop (random 2))
			   (1- value)
			   (1+ value))
		       (1- length)))))

;; (random-walk 100 10)


;; Now we draw random walks into a picture
(defun svg-draw-random-walks ()
  (with-open-file (*standard-output*
		   "random_walk.svg"
		   :direction :output
		   :if-exists :supersede)
    (svg 400 200
      (loop repeat 10
	 do (polygon (append '((0 . 200))
			     (loop for x from 0 ;; bugfix!
				for y in (random-walk 100 400)
				collect (cons x y))
			     '((400 . 200)))
		     (loop repeat 3
			  collect (random 256)))))))
