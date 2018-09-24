(load "dice-of-doom-v3.lisp")

;; Add more players
(defparameter *num-players* 4)
(defparameter *die-colors* '((255 63 63)
			     (63 63 255)
			     (63 255 63)
			     (255 63 255)))

;; AI will use paranoid strategy

(defparameter *max-dice* 5)
(defparameter *ai-level* 2) ;; dumbs down tha AI

;; Tweak to add chance nodes
(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
	     (car (aref board pos)))
	   (dice (pos)
	     (cadr (aref board pos))))
    (lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player)
	   (lazy-mapcan
	    (lambda (dst)
	      (if (and (not (eq (player dst) cur-player))
		       (> (dice src) 1)) ;; This also changed
		  (make-lazy
		   (list
		    (list (list src dst)
			  (game-tree (board-attack board
						   cur-player
						   src
						   dst
						   (dice src))
				     cur-player
				     (+ spare-dice (dice dst))
				     nil)
			  (game-tree (board-attack-fail board ;; Failed attack
							cur-player
							src
							dst
							(dice src))
				     cur-player
				     (+ spare-dice (dice dst))
				     nil))))
		  (lazy-nil)))
	    (make-lazy (neighbors src)))
	   (lazy-nil)))
     (make-lazy (loop for n below *board-hexnum*
		   collect n)))))

(defun board-attack-fail (board player src dst dice)
  (declare (ignore dst dice)) ;; Ignore what we dont use
  (board-array (loop for pos from 0
		  for hex across board
		  collect (if (eq pos src)
			      (list player 1)
			      hex))))

;; Dice rolling

(defun roll-dice (dice-num)
  (let ((total (loop repeat dice-num
		  sum (1+ (random 6)))))
    (fresh-line)
    (format t "On ~a dice rolled ~a. " dice-num total)
    total))

(defun roll-against (src-dice dst-dice)
  (> (roll-dice src-dice)
     (roll-dice dst-dice)))

(defun pick-chance-branch (board move)
  (labels ((dice (pos)
	     (cadr (aref board pos))))
    (let ((path (car move)))
      (if (or (null path)
	      (roll-against (dice (car path))
			    (dice (cadr path))))
	  (cadr move)
	  (caddr move)))))

(defun web-handle-human (pos)
  (cond ((not pos) (princ "Please choose a hex to move from:"))
	((eq pos 'pass)
	 (setf *cur-game-tree*
	       (cadr (lazy-car (caddr *cur-game-tree*))))
	 (princ "Your reinforcements have been placed. ")
	 (tag a (href (make-game-link nil))
	   (princ "continue")))
	((not *from-tile*)
	 (setf *from-tile* pos)
	 (princ "Now choose a destination:"))
	((eq pos *from-tile*)
	 (setf *from-tile* nil)
	 (princ "Move cancelled."))
	(t (setf *cur-game-tree*
		 (pick-chance-branch
		  (cadr *cur-game-tree*)
		  (lazy-find-if (lambda (move)
				  (equal (car move)
					 (list *from-tile* pos)))
				(caddr *cur-game-tree*))))
	   (setf *from-tile* nil)
	   (princ "You may now ")
	   (tag a (href (make-game-link 'pass))
	     (princ "pass"))
	   (princ " or make another move:"))))


(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*)
			      (car tree))))
    (pick-chance-branch (cadr tree)
			(lazy-nth (position (apply #'max ratings) ratings)
				  (caddr tree)))))

;; Update AI

(defparameter *dice-probability* #(#(0.84 0.97 1.00 1.00)
				   #(0.44 0.78 0.94 0.99)
				   #(0.15 0.45 0.74 0.91)
				   #(0.04 0.19 0.46 0.72)
				   #(0.01 0.06 0.22 0.46)))


;; NOTE: This version of Dice of Doom has no alpha-beta prunning
(defun get-ratings (tree player)
  (let ((board (cadr tree)))
    (labels ((dice (pos)
	       (cadr (aref board pos))))
      (take-all (lazy-mapcar
		 (lambda (move)
		   (let ((path (car move)))
		     (if path
			 (let* ((src (car path))
				(dst (cadr path))
				(probability (aref (aref *dice-probability*
							 (1- (dice dst)))
						   (- (dice src) 2))))
			   (+ (* probability (rate-position (cadr move) player))
			      (* (1- probability) (rate-position (caddr move)
								 player))))
			 (rate-position (cadr move) player))))
		 (caddr tree))))))

(defun limit-tree-depth (tree depth)
  (list (car tree)
	(cadr tree)
	(if (zerop depth)
	    (lazy-nil)
	    (lazy-mapcar (lambda (move)
			   (cons (car move)
				 (mapcar (lambda (x)
					   (limit-tree-depth x
							     (1- depth)))
					 (cdr move))))
			 (caddr tree)))))


;; Improve reinforcements rule

(defun get-connected (board player pos)
  (labels ((check-pos (pos visited)
	     (if (and (eq (car (aref board pos))
			  player)
		      (not (member pos visited)))
		 (check-neighbors (neighbors pos)
				  (cons pos visited))
		 visited))
	   (check-neighbors (list visited)
	     (if list
		 (check-neighbors (cdr list)
				  (check-pos (car list)
					     visited))
		 visited)))
    (check-pos pos nil)))

(defun largest-cluster-size (board player)
  (labels ((f (pos visited best)
	     (if (< pos *board-hexnum*)
		 (if (and (eq (car (aref board pos))
			      player)
			  (not (member pos visited)))
		     (let* ((cluster (get-connected board player pos))
			    (size (length cluster)))
		       (if (> size best)
			   (f (1+ pos)
			      (append cluster visited)
			      size)
			   (f (1+ pos)
			      (append cluster visited)
			      best)))
		     (f (1+ pos)
			visited
			best))
		 best)))
    (f 0 nil 0)))


;; Tweak to use new rule
(defun add-new-dice (board player spare-dice)
  (declare (ignore spare-dice))
  (labels ((f (list n)
	     (cond ((zerop n)   list)
		   ((null list) nil)
		   (t (let ((cur-player (caar list))
			    (cur-dice (cadar list)))
			(if (and (eq cur-player player)
				 (< cur-dice *max-dice*))
			    (cons (list cur-player (1+ cur-dice))
				  (f (cdr list) (1- n)))
			    (cons (car list)
				  (f (cdr list) n))))))))
    (board-array (f (coerce board 'list)
		    (largest-cluster-size board player)))))

;; And it is finished!!!!!

;; Start Hunchentoot server:
;; (start-dod-server)

;; Stop Hunchentoot server:
;; (stop-dod-server)
