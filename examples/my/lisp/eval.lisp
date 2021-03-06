(defpackage :eval
  (:use :cl :eql :qml)
  (:export
   #:*eval-thread*
   #:eval-in-thread
   #:ini))

(in-package :eval)

(defvar *output-buffer* (make-string-output-stream))
(defvar *prompt*        t)
(defvar *eval-thread*   nil)
(defvar *               nil)
(defvar **              nil)
(defvar ***             nil)

(defvar *qml-repl-input*   "repl_input")
(defvar *qml-repl-output*  "repl_output")
(defvar *qml-flick-output* "flick_output")
(defvar *qml-progress*     "progress")

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
  (defun eval-in-thread (text &optional (progress t)) ; called from QML
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
        (update-output)
        (when progress
          (show-progress-bar))
        (qsingle-shot 100 (lambda ()
                            (setf *eval-thread*
                                  (mp:process-run-function "EQL5 REPL top-level" (lambda () (do-eval str))))))))))

(defvar *color-values*     "#b0e0ff")
(defvar *color-read-error* "#ffd540")
(defvar *color-error*      "#ffb0b0")

(defun do-eval (str)
  (let ((color *color-read-error*))
    (handler-case
        (let ((exp (read-from-string str)))
          (setf color *color-error*)
          (let ((vals (multiple-value-list (eval exp))))
            (setf *** ** ** * * (first vals))
            (update-output)
            (append-output (format nil "~{~S~^~%~}" vals) *color-values*))
          (qml-call *qml-repl-input* "clear")
          (history-add str))
      (condition (c)
        (show-error c color))))
  (qsingle-shot 100 'eval-exited))

(defun eval-exited ()
  (update-output)
  (show-progress-bar nil)
  (qml-set *qml-flick-output* "contentX" 0))

(defun show-error (error color)
  (let ((e1 (prin1-to-string error))
        (e2 (princ-to-string error)))
    (append-output e1 color)
    (unless (string= e1 e2)
      (append-output e2 color))))

(defun show-progress-bar (&optional (show t))
  (qml-set *qml-progress* "enabled" show)
  (qml-set *qml-progress* "visible" show))

;;; output

(defun update-output ()
  (let ((chunk (get-output-stream-string *output-buffer*)))
    (unless (x:empty-string chunk)
      (let ((text (x:string-substitute "<br>" (string #\Newline) (qescape chunk))))
        ;; "insert" is cleaner with formatting than "append"
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
  (qml-call *qml-repl-output* "append"
            (format nil "<pre><font face='~A' color='~A'>~A</font></pre>"
                    #+android "Droid Sans Mono"
                    #-android "Monospace"
                    color
                    (x:string-substitute "<br>" (string #\Newline) (qescape text)))))

;;; command history

(defvar *history*       (make-array 0 :adjustable t :fill-pointer t))
(defvar *history-index* nil)
(defvar *history-file*  ".eql5-lisp-repl-history")
(defvar *max-history*   100)

(defun read-saved-history ()
  (when (probe-file *history-file*)
    (let ((i -1))
      (labels ((index ()
                 (mod i *max-history*))
               (next-index ()
                 (incf i)
                 (index)))
        (let ((tmp (make-array *max-history*))) ; ring buffer
          (with-open-file (s *history-file* :direction :input)
            (x:while-it (read-line s nil nil)
              (setf (svref tmp (next-index)) x:it)))
          (let ((max (min (1+ i) *max-history*)))
            (when (< max *max-history*)
              (setf i -1))
            (dotimes (n max)
              (vector-push-extend (svref tmp (next-index))
                                  *history*))
            (setf *history-index* (length *history*)))))))) ; 1 after last

(let (out)
  (defun history-ini ()
    (read-saved-history)
    (setf out (open *history-file* :direction :output
                    :if-exists :append :if-does-not-exist :create)))
  (defun history-add (line)
    (unless out
      (history-ini))
    (let ((len (length *history*)))
      (when (or (zerop len)
                (string/= line (aref *history* (1- len))))
        (vector-push-extend line *history*)
        (write-line line out)
        (force-output out)))
    (setf *history-index* (length *history*))) ; 1 after last
  (defun history-move (dir)
    (unless out
      (history-ini))
    (when (and *history-index*
               (plusp (length *history*)))
      (setf *history-index* (if (string= "back" dir)
                                (max (1- *history-index*) 0)
                                (min (1+ *history-index*) (1- (length *history*)))))
      (let ((text (aref *history* *history-index*)))
        (qml-set *qml-repl-input* "text" text)
        (qml-set *qml-repl-input* "cursorPosition"
                 (- (length text) (if (x:ends-with ")" text) 1 0)))))))
