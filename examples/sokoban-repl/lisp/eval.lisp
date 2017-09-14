(defpackage :eval
  (:use :cl :eql :qml)
  (:export
   #:*eval-thread*
   #:ini))

(in-package :eval)

(defvar *qml-repl-input*  "repl_input")
(defvar *qml-repl-output* "repl_output")
(defvar *output-buffer*   (make-string-output-stream))
(defvar *prompt*          t)
(defvar *eval-thread*     nil)
(defvar *                 nil)
(defvar **                nil)
(defvar ***               nil)

(defun ini ()
  (ini-streams))

(defun ini-streams ()
  (setf *standard-output* (make-broadcast-stream *standard-output*
                                                 *output-buffer*))
  (setf *trace-output* *standard-output*
        *error-output* *standard-output*))

(defun current-package-name ()
  (if (eql (find-package :cl-user) *package*)
      "CL-USER"
      (car (sort (list* (package-name *package*) (package-nicknames *package*))
                 (lambda (x y) (< (length x) (length y)))))))

(let ((n 0))
  (defun eval-in-thread (text) ; called from QML
    (let ((str (string-trim " " text)))
      (unless (x:empty-string str)
        (if *prompt*
            (let ((pkg (if (zerop n) "EQL-USER" (current-package-name)))
                  (counter (princ-to-string (incf n))))
              (format t "~%~A [~A] ~A~%~A"
                      pkg
                      counter
                      (make-string (- 50 (length counter) (length pkg)) :initial-element #\-)
                      str))
            (format t "~A~%~%~A" #.(make-string 50 :initial-element #\_) str))
        ;; run eval in its own thread, so UI will remain responsive
        ;; N.B. this is only safe because we use "thread-safe.lisp" (like in Slime mode)
        (setf *eval-thread* (mp:process-run-function "EQL5 REPL top-level" (lambda () (do-eval str))))))))

(defvar *color-values*     "skyblue")
(defvar *color-read-error* "orange")
(defvar *color-error*      "#ffb0b0")

(defun do-eval (str)
  (start-output-timer)
  (let ((color *color-read-error*))
    (handler-case
        (let ((exp (read-from-string str)))
          (setf color *color-error*)
          (let ((vals (multiple-value-list (eval exp))))
            (setf *** ** ** * * (first vals))
            (append-output (format nil "~{~S~^~%~}" vals) *color-values*))
          (qml-call *qml-repl-input* "clear")
          (history-add str))
      (condition (c)
        (show-error c color))))
  (update-output)
  (stop-output-timer))

(defun show-error (error color)
  (let ((e1 (prin1-to-string error))
        (e2 (princ-to-string error)))
    (append-output e1 color)
    (unless (string= e1 e2)
      (append-output e2 color))))

;;; output

(defvar *output-timer* nil)

(defun start-output-timer (&optional (interval 500))
  (unless *output-timer*
    (setf *output-timer* (qnew "QTimer"))
    (qconnect *output-timer* "timeout()" 'update-output))
  (|start| *output-timer* interval))

(defun stop-output-timer ()
  (|stop| *output-timer*))

(defun update-output ()
  (let ((chunk (get-output-stream-string *output-buffer*)))
    (unless (x:empty-string chunk)
      (let ((text (x:string-substitute "<br>" (string #\Newline) (qescape chunk))))
        (qml-call *qml-repl-output* "insert"
                  (qml-get *qml-repl-output* "length")
                  (format nil "<pre><font face='~A'>~A</font></pre>"
                          #+android "Droid Sans Mono"
                          #-android "Monospace"
                          (x:if-it (search "[EQL:err]" text)
                                   (let ((error-text (subseq text x:it)))
                                     (x:string-substitute (format nil "<font color='~A'>~A</font>"
                                                                  *color-error*
                                                                  error-text)
                                                          error-text
                                                          text))
                                   text))))
      (qml-set *qml-repl-output* "cursorPosition"
               (qml-get *qml-repl-output* "length")))))

(defun append-output (text &optional (color "white"))
  (update-output)
  (qml-call *qml-repl-output* "append"
            (format nil "<pre><font face='~A' color='~A'>~A</font></pre>"
                    #+android "Droid Sans Mono"
                    #-android "Monospace"
                    color
                    (x:string-substitute "<br>" (string #\Newline) (qescape text)))))

;;; command history

(defvar *max-history*  100)
(defvar *history-file* ".eql5-lisp-repl-history")

(defun saved-history ()
  (let ((ex "")
        history)
    (when (probe-file *history-file*)
      (with-open-file (s *history-file* :direction :input)
        (x:while-it (read-line s nil nil)
          (when (string/= ex x:it)
            (setf ex x:it)
            (push x:it history))))
      (setf history (nthcdr (max 0 (- (length history) *max-history*)) (reverse history)))
      (with-open-file (s *history-file* :direction :output
                         :if-exists :supersede)
        (dolist (cmd history)
          (write-line cmd s)))
      (reverse history))))

(let ((ex "up")
      up down out)
  (defun history-ini ()
    (setf up  (saved-history)
          out (open *history-file* :direction :output
                    :if-exists :append :if-does-not-exist :create)))
  (defun history-move (direction)
    (setf *file* nil)
    (unless out
      (history-ini))
    (let (exp)
      (dotimes (n (if (string= direction ex) 1 2))
        (setf exp (cond ((string= "up" direction)
                         (x:when-it (pop up)
                           (push x:it down)))
                        ((string= "down" direction)
                         (x:when-it (pop down)
                           (push x:it up))))))
      (setf ex direction)
      (when exp
        (qml-set *qml-repl-input* "text" (first exp)))))
  (defun history-add (cmd)
    (unless out
      (history-ini))
    (when (or (not up)
              (and up (string/= cmd (first up))))
      (push cmd up)
      (princ cmd out)
      (terpri out)
      (force-output out)
      (when (and down (string= cmd (first down)))
        (pop down))))
  (defun history ()
    (append (reverse up) down)))
