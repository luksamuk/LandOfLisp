;;; Chapter 10

;; Cartesian product between multiple ranges
;; Generate 10 lists of 10 elements
(defun cartesian ()
  (loop for x below 10
     collect (loop for y below 10
                  collect (+ x y))))

(defun track-index ()
  (loop for i from 0
     for day in '(monday tuesday wednesday
                  thursday friday saturday sunday)
       collect (cons i day)))

;; Periodic Table of the Loop Macro: Check page 200-201!
