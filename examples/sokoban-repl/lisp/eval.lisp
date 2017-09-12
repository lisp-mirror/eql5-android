(defpackage :eval
  (:use :cl :eql :qml)
  (:export
   #:*eval-thread*
   #:ini))

(in-package :eval)

(defvar *qml-input*     "repl_input")
(defvar *qml-output*    "output")
(defvar *output-buffer* (make-string-output-stream))
(defvar *prompt*        t)
(defvar *eval-thread*   nil)
(defvar *               nil)
(defvar **              nil)
(defvar ***             nil)

(defun ini ()
  (ini-streams))

(defun ini-streams ()
  (setf *trace-output* *standard-output*
        *error-output* *standard-output*) 
  (setf *standard-output* (make-broadcast-stream *standard-output*
                                                 *output-buffer*)))

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

(defun do-eval (str)
  (start-output-timer)
  (let ((color "orange"))
    (handler-case
        (let ((exp (read-from-string str)))
          (setf color "#ffb0b0")
          (let ((vals (multiple-value-list (eval exp))))
                (setf *** ** ** * * (first vals))
                (append-output (format nil "~{~S~^~%~}" vals) "skyblue"))
          (history-add str))
      (error (err)
        (show-error err color))))
  (update-output)
  (stop-output-timer))

(defun show-error (err color)
  (let ((e1 (prin1-to-string err))
        (e2 (princ-to-string err)))
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
      (qml-call *qml-output* "insert"
                (qml-get *qml-output* "length")
                (format nil "<pre><font face='~A'>~A</font></pre>"
                        #+android "Droid Sans Mono"
                        #-android "Monospace"
                        (x:string-substitute "<br>" (string #\Newline) (qescape chunk))))
      (qml-set *qml-output* "cursorPosition" (qml-get *qml-output* "length")))))

(defun append-output (text &optional (color "white"))
  (update-output)
  (qml-call *qml-output* "append"
            (format nil "<pre><font face='~A' color='~A'>~A</font></pre>"
                    #+android "Droid Sans Mono"
                    #-android "Monospace"
                    color
                    (x:string-substitute "<br>" (string #\Newline) (qescape text)))))

;;; command history

(defvar *max-history*  100)
(defvar *history-file* nil)

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
      down out)
  (defun history-ini ()
    (setf *history-file* ".eql5-lisp-repl-history")
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
        (qml-set *qml-input* "text" (first exp)))))
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

