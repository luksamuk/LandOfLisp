;;; Orc Battle Game (Chapter 9)

;; Player variables
(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

;; Monster variables
(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)


(defun orc-battle ()
  "Effectively starts game and runs game loop."
  (init-monsters) ; to-do
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead) ; to-do
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead)
             (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility* 15)))))
      (unless (monsters-dead)
        (show-monsters)
        (player-attack)))
    (fresh-line)
    (map 'list
         (lambda (m)
           (or (monster-dead m)
              (monster-attack m)))
         *monsters*)
    (game-loop)))


;; Player functions
(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a health of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab  [d]ouble swing  [r]oundhouse: ")
  (case (read)
    (s (monster-hit (pick-monster) ; stab
                    (+ 2 (randval (ash *player-strength* -1)))))
    (d (let ((x (randval (truncate (/ *player-strength* 6)))))
         (princ "Your double swing has a strength of ") ; double swing
         (princ x)
         (fresh-line)
         (monster-hit (pick-monster) x)
         (unless (monsters-dead)
           (monster-hit (pick-monster) x))))
    (otherwise ; roundhouse
     (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
       (unless (monsters-dead)
         (monster-hit (random-monster 1)))))))

(defun randval (n)
  "Retrieves an inclusive random value between 1 and n."
  (1+ (random (max 1 n))))


(defun random-monster ()
  "Picks up a random monster that isn't already dead."
  (let ((m (aref *monsters* (random (length *monsters*)))))
    (if (monster-dead m)
        (random-monster)
        m)))


(defun pick-monster ()
  "Picks up a nonrandom monster."
  (fresh-line)
  (princ "Monster #: ")
  (let ((x (read)))
    (if (not (and (integerp x)
              (>= x 1)
              (<= x *monster-num*)))
        (progn (princ "That is not a valid monster number.")
               (pick-monster))
        (let ((m (aref *monsters* 1- x)))
          (if (monster-dead m)
              (progn (princ "That monster is already dead.")
                     (pick-monster))
              m)))))



;; Monster management

(defun init-monsters ()
  (setf *monsters*
        (map 'vector
             (lambda (x)
               (funcall (nth (random (length *monster-builders*))
                             *monster-builders*)))
             (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes: ")
  (let ((x 0))
    (map 'list
         (lambda (m)
           (fresh-line)
           (princ "  ")
           (princ (incf x))
           (princ ", ")
           (if (monster-dead m)
               (princ "**dead**")
               (progn (princ "(Health=")
                      (princ (monster-health m))
                      (princ ") ")
                      (monster-show m))))
         *monsters*)))


