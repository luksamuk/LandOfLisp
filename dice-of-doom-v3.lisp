(load "dice-of-doom-v2.lisp")
(load "web-server.lisp")
(load "svg.lisp")

(defparameter *board-width* 900)
(defparameter *board-height* 500)
(defparameter *board-scale* 64)
(defparameter *top-offset* 3)
(defparameter *dice-scale* 40)
(defparameter *dot-size* 0.05)

(defun draw-die-svg (x y col)
  (labels ((calc-pt (pt)
	     (cons (+ x (* *dice-scale* (car pt)))
		   (+ y (* *dice-scale* (cdr pt)))))
	   (f (pol col)
	     (polygon (mapcar #'calc-pt pol) col)))
    (f '(( 0.0 . -1.0)
	 (-0.6 . -0.75)
	 ( 0.0 . -0.5)
	 ( 0.6 . -0.75))
       (brightness col 40))
    (f '(( 0.0 . -0.5)
	 (-0.6 . -0.75)
	 (-0.6 .  0.0)
	 ( 0.0 .  0.25))
       col)
    (f '(( 0.0 . -0.5)
	 ( 0.6 . -0.75)
	 ( 0.6 .  0.0)
	 ( 0.0 .  0.25))
       (brightness col -40))
    (mapc (lambda (x y)
	    (polygon (mapcar (lambda (xx yy)
			       (calc-pt (cons (+ x (* xx *dot-size*))
					      (+ y (* yy *dot-size*)))))
			     '(-1.0 -1.0 1.0  1.0)
			     '(-1.0  1.0 1.0 -1.0))
		     '(255 255 255)))
	  '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2)
	  '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625
	    -0.35 -0.05 -0.45 -0.15 -0.45 -0.05))))

;; Test:
;; (with-open-file (*standard-output*
;; 		 "die.svg"
;; 		 :direction :output
;; 		 :if-exists :supersede)
;;   (svg 100 100 (draw-die-svg 50 50 '(255 0 0))))

(defun draw-tile-svg (x y pos hex xx yy col chosen-tile)
  (loop for z below 2
     do (polygon (mapcar (lambda (pt)
			   (cons (+ xx (* *board-scale* (car pt)))
				 (+ yy (* *board-scale*
					  (+ (cdr pt) (* (- 1 z) 0.1))))))
			 '((-1.0 . -0.2) ( 0.0 . -0.5) ( 1.0 . -0.2)
			   ( 1.0 .  0.2) ( 0.0 .  0.5) (-1.0 .  0.2)))
		 (if (eql pos chosen-tile)
		     (brightness col 100)
		     col)))
  (loop for z below (second hex)
     do (draw-die-svg (+ xx
			 (* *dice-scale*
			    0.3
			    (if (oddp (+ x y z))
				-0.3
				0.3)))
		      (- yy (* *dice-scale* z 0.8))
		      col)))
;; Test:
;; (with-open-file (*standard-output*
;; 		 "tile.svg"
;; 		 :direction :output
;; 		 :if-exists :supersede)
;;   (svg 300 300 (draw-tile-svg 0 0 0 '(0 3) 100 150 '(255 0 0) nil)))

(defparameter *die-colors* '((255 63 63) (63 63 255)))

(defun draw-board-svg (board chosen-tile legal-tiles)
  (loop for y below *board-size*
     do (loop for x below *board-size*
	   for pos = (+ x (* *board-size* y))
	   for hex = (aref board pos)
	   for xx = (* *board-scale* (+ (* 2 x) (- *board-size* y)))
	   for yy = (* *board-scale* (+ (* y 0.7) *top-offset*))
	   for col = (brightness (nth (first hex) *die-colors*)
				 (* -15 (- *board-size* y)))
	   do (if (or (member pos legal-tiles)
		      (eql pos chosen-tile))
		  (tag g ()
		    (tag a ("xlink:href" (make-game-link pos))
		      (draw-tile-svg x y pos hex xx yy col chosen-tile)))
		  (draw-tile-svg x y pos hex xx yy col chosen-tile)))))

(defun make-game-link (pos)
  (format nil "/game.html?chosen=~a" pos))

;; Test:
;; (with-open-file (*standard-output*
;; 		 "board.svg"
;; 		 :direction :output
;; 		 :if-exists :supersede)
;;   (svg *board-width* *board-height*
;;     (draw-board-svg (gen-board) nil nil)))
