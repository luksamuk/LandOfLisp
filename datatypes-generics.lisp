(defstruct person
  name
  age
  waist-size
  favorite-color)

(defparameter *bob* (make-person :name "Bob"
                                 :age 35
                                 :waist-size 32
                                 :favorite-color "blue"))

;; print/read symmetry example.
;; we can also define a struct with the same syntax
;; it prints on the REPL
(defparameter *that-guy*
  #s(person :name "Bob" :age 35 :waist-size 32 :favorite-color "blue"))


;; find largest even number in list
(reduce (lambda (best item)
          (if (and (evenp item)
                 (> item best))
              item
              best))
        '(7 4 6 5 2)
        :initial-value 0)

(defun sum (list)
  (reduce #'+ list))


;; map works for any sequence and you also need to specify
;; which type you wish to return
(map 'string ;; the book uses 'list. We may also use 'vector
     (lambda (x)
       (if (eq x #\s)
           #\S
           x))
     "this is a string")


;; add two numbers
(defun add (a b)
  (cond ((and (numberp a)
            (numberp b))
         (+ a b))
        ((and (listp a)
            (listp b))
         (append a b))))

;; erm, that's not really efficient nor recommended,
;; let's try that again.

;; forget the symbol
(fmakunbound 'add)

;; implement as methods
(defmethod add ((a number) (b number))
  (+ a b))

(defmethod add ((a list) (b list))
  (append a b))
