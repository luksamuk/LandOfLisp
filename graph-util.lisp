;; ================= dot visualization ======================
;; NOTE: These should go to a file graph-util.lisp.

(defparameter *max-label-length* 30)

(defun dot-name (exp)
  ;; Predicate which substitutes based on result of test function
  ;; Can also be used on lists, therefore is also is a generic function
  ;; (multiple datatypes)
  (substitute-if #\_ ; underline character
                 (complement #'alphanumericp) ; creates an opposite (complement) predicate from another
                 (prin1-to-string exp))) ; atom -> string

;; There is also substitute-if-not, but -not functions are considered deprecated.


;; Generates label with no more than *max-label-length* characters
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

;; Convert alist of nodes into dot format
(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

;; Generate edges data
(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
          edges))

;; Generate ALL dot data
(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))


;; Create a picture using the dot information
;; This one is adapted so it works with SBCL.
(defun dot->png (fname thunk)
  ;; Trivia: A thunk/suspension/nullary func is a function which
  ;; take no arguments and can run later.
  (with-open-file (*standard-output*
                   fname
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  ;;(ext:shell (concatenate 'string "dot -Tpng -O " fname)) ; CLISP
  (sb-ext:run-program "/usr/bin/dot" (list "-Tpng" "-O" fname))) ; SBCL

;; Ties everything together and writes the PNG file
(defun graph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (graph->dot nodes edges))))


;; Drawing undirected graphs
(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))
