;; We need to use usocket from Quicklisp here.
;; So yeah... yeah.
(ql:quickload 'usocket)

;; WARNING: DO NOT USE THIS CODE!
;; This implementation incurs in broken pipes everywhere.
;; Also, it is not suited for modern browsers. This implementation
;; ends up either rendered as plain text or end up with a broken
;; pipe.
;; For a solid way to serve programatically-generated HTML in
;; Common Lisp, see HUNCHENTOOT
;; (http://quickdocs.org/hunchentoot/api)


;; Links and requests parser

(defun http-char (c1 c2 &optional (default #\space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (f (cdddr lst))))
                 (#\+ (cons #\space (f (cdr lst))))
                 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list))
            'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x)
              (parse-params (subseq url (1+ x))))
        (cons url '()))))

;; Parse header of request
(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

;; Parse contents of request
(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

;; Finally, SERVE.
(defun serve (request-handler)
  (let ((socket (usocket:socket-listen "127.0.0.1" 8080)))
    (unwind-protect
         (loop (with-open-stream (stream (usocket:socket-stream
                                          (usocket:socket-accept socket)))
                 (let* ((url    (parse-url (read-line stream)))
                        (path   (car url))
                        (header (get-header stream))
                        (params (append (cdr url)
                                        (get-content-params stream header)))
                        (*standard-output* stream))
                   (funcall request-handler path header params))))
      (usocket:socket-close socket))))


(defparameter name-getting-page
  (format nil
          (concatenate 'string
                       "<!DOCTYPE html>~%"
                       "<html lang='en'>~%"
                       "  <head></head>~%"
                       "  <body>~%"
                       "    <form>What is your name?<input name='name' /></form>~%"
                       "  </body>~%"
                       "</html>~%")))


;; Simple website handler
;; Notice that I changed stuff here from the book! This is because modern browsers
;; will enclose everything on an html body.
;; One thing, though... unfortunately, it is displaying stuff as verbatim in
;; Firefox.
(defun hello-request-handler (path header params)
  (declare (ignore header)) ; so we don't trigger warnings
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
        (if (not name)
            (princ name-getting-page)
            (format t "Nice to meet you, ~a!" (cdr name))))
      (princ "Sorry... I don't know that page.")))
