(load "dice-of-doom.lisp")
(load "lazy.lisp")

(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;; Rewritten functions for proper lazy evaluation

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil ;; use lazy-cons
		       (game-tree (add-new-dice board player (1- spare-dice))
				  (mod (1+ player) *num-players*)
				  0
				  t))
		 moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
	     (car (aref board pos)))
	   (dice (pos)
	     (cadr (aref board pos))))
    (lazy-mapcan ;; use lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player) ;; trade when for if
	   (lazy-mapcan ;; use lazy-mapcan
	    (lambda (dst)
	      (if (and (not (eq (player dst) cur-player)) ;; trade when for if
		       (> (dice src) (dice dst)))
		  (make-lazy ;; add make-lazy for list
		   (list
		    (list (list src dst)
			  (game-tree (board-attack board
						   cur-player
						   src
						   dst
						   (dice src))
				     cur-player
				     (+ spare-dice (dice dst))
				     nil))))
		  (lazy-nil))) ;; add lazy-nil as else-statement
	    (make-lazy (neighbors src))) ;; lazy neighbors
	   (lazy-nil))) ;; add lazy-nil as else-statement
     (make-lazy (loop for n below *board-hexnum* ;; lazy list of board pos
		   collect n)))))


(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
	       (unless (lazy-null moves)
		 (let* ((move (lazy-car moves))
			(action (car move)))
		   (fresh-line)
		   (format t "~a. " n)
		   (if action
		       (format t "~a -> ~a" (car action) (cadr action))
		       (princ "end turn")))
		 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

;; Test by evaluating this
;; (play-vs-human (game-tree (gen-board) 0 0 t))
