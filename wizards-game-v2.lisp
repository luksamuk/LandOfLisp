(load "wizards-game.lisp")

(defun have (object)
  (member object (cdr (inventory))))

(defparameter *chain-welded* nil)

;; (defun weld (subject object)
;;   (if (and (eq *location* 'attic)
;; 	   (eq subject 'chain)
;; 	   (eq object 'bucket)
;; 	   (have 'chain)
;; 	   (have 'bucket)
;; 	   (not *chain-welded*))
;;       (progn (setf *chain-welded* t)
;; 	     '(the chain is now securely welded to the bucket.))
;;       '(you cannot weld like that.)))

;; (pushnew 'weld *allowed-commands*)



(defparameter *bucket-filled* nil)

;; (defun dunk (subject object)
;;   (if (and (eq *location* 'garden)
;; 	   (eq subject 'bucket)
;; 	   (eq object 'well)
;; 	   (have 'bucket)
;; 	   *chain-welded*)
;;       (progn (setf *bucket-filled* t)
;; 	     '(the bucket is now full of water))
;;       '(you cannot dunk like that.)))

;; (defmacro game-action (command subj obj place &body body)
;;   `(progn (defun ,command (subject object)
;; 	    (if (and (eq *location* ',place)
;; 		     (eq subject ',subj)
;; 		     (eq object ',obj)
;; 		     (have ',subj))
;; 		,@body
;; 		'(i cant ,command like that.)))
;; 	  (pushnew ',command *allowed-commands*)))


;; EXERCISE, p. 371: Avoiding the name clash of 'subject' and 'object'.

(defmacro game-action (command subj obj place &body body)
  (let ((g1 (gensym))
	(g2 (gensym)))
    `(progn (defun ,command (,g1 ,g2)
	      (if (and (eq *location* ',place)
		       (eq ,g1 ',subj)
		       (eq ,g2 ',obj)
		       (have ',subj))
		  ,@body
		  '(i cant ,command like that.)))
	    (pushnew ',command *allowed-commands*))))

;; Rewriting game commands using game-action macro

(game-action weld chain bucket attic
  (if (and (have 'bucket)
	   (not *chain-welded*))
      (progn (setf *chain-welded* t)
	     '(the chain is now securely welded to the bucket.))
      '(you do not have a bucket.)))


(game-action dunk bucket well garden
  (if *chain-welded*
      (progn (setf *bucket-filled* t)
	     '(the bucket is now full of water.))
      '(the water level is too low to reach.)))


;; More commands

(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*)
	 '(the bucket has nothing in it.))
	((have 'frog)
	 '(the wizard awakens and sees that you stole his frog.
	   he is so upset he banishes you to the netherworlds-
	   you lose! the end.))
	(t '(the wizard awakens from his slumber and greets you warmly.
	     he hands you the magic low-carb donut- you win! the end.))))



