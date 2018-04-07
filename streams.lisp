;;;;; Chapter 12

;; Writing an alist to a file
(let ((animal-noises '((dog . woof)
                       (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output
                             :if-exists :supersede)
    (print animal-noises my-stream)))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))


;;;; Using sockets
;; I've changed these calls so they match SBCL. So these will only
;; work on SBCL, as it is suggested.

;;; SERVER-SIDE
;; We create a socket. Instead of (socket-server port), SBCL
;; offers no default socket implementation, so we'll reccur to
;; quicklisp.
;; GOOD THING I ALREADY KNOW HOW TO USE QUICKLISP. HA!
;; Though I'm not configuring an ASDF project only for this.
;; Eval it on SLIME or whatever.

(ql:quickload "usocket")
(use-package :usocket)

;; Create the server's socket
;; (socket-server 4321) => (socket-listen host port)
(defparameter my-socket (socket-listen "127.0.0.1" 4321))

;; Make a stream.
;; usocket uses the same syntax.
;; NOTE: You may want to type this directly on the REPL;
;; also, this will lock it up.
(defparameter my-stream (socket-accept my-socket))



;;; CLIENT-SIDE
;; Use the commented code below.
;; Notice the slight change in relation to the book's
;; parameter order.
;; * (defparameter my-stream (socket-connect "127.0.0.1" 4321))

;; The server should now unlock!

;; Now, on the client, we can output whatever we want, whenever
;; we want, to our server, using the stream as any other stream.
;; The stream is bidirectional, also.

;; To send messages on the client, for example:
;; * (print "Yo, server!" (socket-stream my-stream))
;; * (force-output (socket-stream my-stream))

;; Notice that we needed to
;; 1. Use an accessor to access the actual stream;
;; 2. Force an output flush to the stream.

;; To receive them here:
(read (socket-stream my-stream))

;; We also need to use the accessor.


;;; SENDING TO CLIENT
;; Now we can also send stuff to the client:
(progn
  (print "Wassup, Client!" (socket-stream my-stream))
  (force-output (socket-stream my-stream)))

;; Do the same as we did before to receive the output on client.
;; * (read (socket-stream my-stream))


;; CLEANUP
;; Run the following command on BOTH the SERVER and the CLIENT:
(close (socket-stream my-stream))

;; And now, we effectively destroy the socket for this server:
(socket-close my-socket)

;; More documentation: https://common-lisp.net/project/usocket/api-docs.shtml




;;;; Using string streams
;; Output stream example directly copied from the book

(defparameter foo (make-string-output-stream))
(princ "This will go into foo. " foo)
(princ "This will also go into foo. " foo)
(get-output-stream-string foo) ; Get result and clear up the stream

;; Extra macro to look at: with-output-to-string.
;; Can be better than just concatenating stuff!
(with-output-to-string (*standard-output*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 2 5)))
