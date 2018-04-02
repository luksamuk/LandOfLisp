;; NOTE: Since this is a type-in game, I doubt that there
;; is much to learn here (other than the painful experience
;; of copying format strings). I'm gonna leave that fancy
;; format string for printing a number table from page
;; 232 here, though:
;;
;;(format t
;;        "|~{~<|~%|~,33:;~2d ~>~}|"
;;        (loop for x below 100 collect x))
;;

(defun robots ()
  (loop named main ; by naming it main, we can use "return from" to exit early
     with directions = '((q . -65) (w . -64) (e . -63)
                         (a . -1) (d . 1) ; directions to walk
                         (z . 63) (x . 64) (c . 65))
     for pos = 544
     then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave: ")
                 (force-output)
                 (let* ((c (read))
                        (d (assoc c directions))) ; lookup walk direction
                   ;; Game board is 64x16
                   (cond (d (+ pos (cdr d)))
                         ((eq 't c) (random 1024))
                         ((eq 'l c) (return-from main 'bye)) ; leave game
                         (t pos))))
     for monsters = (loop repeat 10
                       collect (random 1024))
     then (loop for mpos in monsters
             collect
               (if (> (count mpos monsters) 1)
                   mpos
                   ;; Sort locations based on player distance
                   ;; then chomp off the closest
                   (cdar (sort (loop for (k . d) in directions
                                  for new-mpos = (+ mpos d)
                                  ;; Calculate "manhattan distance" to player
                                  collect (cons (+ (abs (- (mod new-mpos 64)
                                                           (mod pos 64)))
                                                   (abs (- (ash new-mpos -6)
                                                           (ash pos -6))))
                                                new-mpos))
                               '<
                               :key #'car))))
     ;; Check if all monsters are scrap
     when (loop for mpos in monsters
             always (> (count mpos monsters) 1)) ; > 1 bot in mpos = scrap
     return 'player-wins
     do (format t
                "~%|~{~<|~%|~,65:;~A~>~}|" ; crraazy. See fancy command on top
                (loop for p
                   below 1024 ; loop thru board positions
                   collect
                     (cond ((member p monsters)
                            (cond ((= p pos)
                                   (return-from main 'player-loses)) ; pl + bot = ded
                                  ((> (count p monsters) 1) #\#)
                                  (t #\A)))
                           ((= p pos) #\@)
                           (t #\ ))))))
