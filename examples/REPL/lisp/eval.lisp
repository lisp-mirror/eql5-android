(defpackage :eval
  (:use :cl :eql :qml)
  (:export
   #:*eval-thread*
   #:*query-dialog*
   #:*debug-dialog*
   #:ini
   #:feed-top-level))

(in-package :eval)

(defvar *standard-output-buffer* (make-string-output-stream))
(defvar *trace-output-buffer*    (make-string-output-stream))
(defvar *error-output-buffer*    (make-string-output-stream))
(defvar *terminal-out-buffer*    (make-string-output-stream))
(defvar *prompt*                 t)
(defvar *silent*                 nil)
(defvar *debug-invoked*          nil)
(defvar *query-invoked*          nil)
(defvar *eval-thread*            nil)
(defvar *gui-debug-io*           nil)
(defvar *gui-output*             nil)
(defvar *gui-query-dialog*       nil)
(defvar *gui-debug-dialog*       nil)

(defvar eql::*reloading-qml*     nil)

(defvar *qml-eval*     "eval")
(defvar *qml-progress* "progress")

(defun ini (&key output query-dialog debug-dialog)
  (setf *gui-output*       output
        *gui-query-dialog* query-dialog
        *gui-debug-dialog* debug-dialog)
  (ini-streams)
  (setf *debug-io* *gui-debug-io*))

(defun ini-streams ()
  (if *silent*
      (setf *standard-output* *standard-output-buffer*
            *trace-output*    *trace-output-buffer*
            *error-output*    *error-output-buffer*)
      (setf *standard-output* (make-broadcast-stream *standard-output*
                                                     *standard-output-buffer*)
            *trace-output*    (make-broadcast-stream *trace-output*
                                                     *trace-output-buffer*)
            *error-output*    (make-broadcast-stream *error-output*
                                                     *error-output-buffer*)))
  (setf *terminal-io*  (make-two-way-stream (two-way-stream-input-stream *terminal-io*)
                                            (if *silent*
                                                *terminal-out-buffer*
                                                (make-broadcast-stream (two-way-stream-output-stream *terminal-io*)
                                                                       *terminal-out-buffer*)))
        *query-io*     (make-two-way-stream (input-hook:make 'handle-query-io)
                                            (two-way-stream-output-stream *terminal-io*))
        *gui-debug-io* (make-two-way-stream (input-hook:make 'handle-debug-io)
                                            (two-way-stream-output-stream *terminal-io*))))

(defun current-package-name ()
  (if (eql (find-package :cl-user) *package*)
      "CL-USER"
      (car (sort (list* (package-name *package*) (package-nicknames *package*))
                 (lambda (x y) (< (length x) (length y)))))))

(defun write-output (type var)
  (let ((str (get-output-stream-string var)))
    (unless (x:empty-string str)
      (when (eql :output type)
        (let ((pos-gt (position #\> str))
              (pos-nl (position #\Newline str)))
          ;; cut off prompt (always present on e.g. android)
          ;; otherwise cut off newline only
          (setf str (subseq str (cond (pos-gt (+ pos-gt 2))
                                      (pos-nl (+ pos-nl 1))
                                      (t 0))))))
      (when (and *gui-output*
                 (not (x:empty-string str)))
        (funcall *gui-output* type str)))))

(let ((n 0))
  (defun feed-top-level (str)
    (unless (x:empty-string str)
      (if *prompt*
          (let ((pkg (if (zerop n) "EQL-USER" (current-package-name)))
                (counter (princ-to-string (incf n))))
            (format t "~A [~A] ~A~%~A"
                    pkg
                    counter
                    (make-string (- 50 (length counter) (length pkg)) :initial-element #\-)
                    str))
          (format t "~A~%~%~A" #.(make-string 50 :initial-element #\_) str))
      (multiple-value-bind (x end)
          (ignore-errors (read-from-string str))
        ;; use LOAD if there is more than 1 form
        (setf si::*read-string*
              (if (eql (length str) end)
                  str
                  (format nil "(load (make-string-input-stream ~S))" str))))
      ;; run eval in its own thread, so GUI will remain responsive
      ;; N.B. this is only safe because we use "thread-safe.lisp" (like in Slime mode)
      (set-eval-state t)
      (setf *debug-invoked* nil
            *query-invoked* nil)
      (write-output :expression *standard-output-buffer*)
      (qsingle-shot 100 (lambda ()
                          (setf *eval-thread*
                                (mp:process-run-function "REPL eval" 'start-top-level)))))))

(defun start-top-level ()
  (si::%top-level)
  (setf *eval-thread* nil)
  (qsingle-shot 100 'top-level-exited))

(defun top-level-exited ()
  (write-output :trace  *trace-output-buffer*)
  (write-output :output *standard-output-buffer*)
  (write-output :error  *error-output-buffer*)
  (when (and *gui-output*
             (or (not *debug-invoked*)
                 (and *debug-invoked*
                      *query-invoked*)))
    (funcall *gui-output* :values (format nil "~{~S~^#||#~}" si::*latest-values*))) ; "#||#": separator
  (unless eql::*reloading-qml*
    (set-eval-state nil)))

(defun show-progress-bar (&optional (show t))
  (qml-set *qml-progress* "enabled" show)
  (qml-set *qml-progress* "visible" show))

(defun set-eval-state (evaluating)
  (unless eql::*reloading-qml*
    (qml-set *qml-eval* "enabled"
             (if (find-package :swank) t (not evaluating)))
    (show-progress-bar evaluating)))

(defun handle-query-io ()
  (setf *query-invoked* t)
  (let ((text (funcall *gui-query-dialog* (get-output-stream-string *terminal-out-buffer*))))
    (when (and *gui-output*
               (not (x:empty-string text)))
      (funcall *gui-output* :values text))
    (format nil "~A~%" text)))

(defun find-quit-restart ()
  ;; find best restart for ':q' (default), to exit the debugger immediately (if possible)
  (let ((restarts (compute-restarts)))
    (if (= 1 (length restarts))
        ":r1"
        (let ((restart-names (mapcar (lambda (r) (symbol-name (restart-name r))) ; N.B. first is RESTART-DEBUGGER
                                     restarts)))
          ;; precedence role
          (dolist (name '("RESTART-TOPLEVEL"
                          "ABORT"
                          "RESTART-QT-EVENTS"))
            (x:when-it (position name restart-names :test 'string=)
              (return-from find-quit-restart (format nil ":r~D" x:it)))))))      ; for index, see above
  ":q")

(defun handle-debug-io ()
  (set-eval-state nil)
  (setf *debug-invoked* t)
  (let ((cmd (funcall *gui-debug-dialog* (list (cons (get-output-stream-string *error-output-buffer*) "#d00000")
                                               (cons (get-output-stream-string *terminal-out-buffer*) "black")))))
    (when (string-equal ":q" cmd)
      (setf cmd (find-quit-restart)))
    (format nil "~A~%" (if (x:empty-string cmd) ":q" cmd))))
