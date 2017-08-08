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
(defvar *silent*                 t)
(defvar *debug-invoked*          nil)
(defvar *eval-thread*            nil)
(defvar *gui-debug-io*           nil)
(defvar *gui-output*             nil)
(defvar *gui-query-dialog*       nil)
(defvar *gui-debug-dialog*       nil)

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
        *query-io*     (make-two-way-stream (input-hook:new 'handle-query-io)
                                            (two-way-stream-output-stream *terminal-io*))
        *gui-debug-io* (make-two-way-stream (input-hook:new 'handle-debug-io)
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
      (setf si::*read-string* (format nil "(progn ~A)" str))
      ;; run eval in its own thread, so GUI will remain responsive
      ;; N.B. this is only safe because we use "thread-safe.lisp" (like in Slime mode)
      (setf *eval-thread* (mp:process-run-function "top-level" 'start-top-level)))))

(defun start-top-level ()
  (qml:qml-set "eval" "enabled" "false")
  (qml:qml-set "eval" "text" "<font color='blue'><b>Evaluating</b></font>")
  (setf *debug-invoked* nil)
  (write-output :expression *standard-output-buffer*)
  (clear-buffers)
  (si::%top-level)
  (write-output :error  *error-output-buffer*)
  (write-output :trace  *trace-output-buffer*)
  (write-output :output *standard-output-buffer*)
  (when *gui-output*
    (funcall *gui-output* :values (format nil "~{~S~^#||#~}" si::*latest-values*)) ; "#||#": separator
    (setf si::*latest-values* nil))
  (qml:qml-set "eval" "text" "<b>Eval</b>")
  (qml:qml-set "eval" "enabled" "true"))

(defun clear-buffers ()
  (get-output-stream-string *standard-output-buffer*)
  (get-output-stream-string *error-output-buffer*)
  (get-output-stream-string *terminal-out-buffer*))

(defun handle-query-io ()
  (let ((text (funcall *gui-query-dialog* (get-output-stream-string *terminal-out-buffer*))))
    (when *gui-output*
      (funcall *gui-output* :values text))
    (format nil "~A~%" text)))

(defun handle-debug-io ()
  (setf *debug-invoked* t)
  (let ((cmd (funcall *gui-debug-dialog* (list (cons (get-output-stream-string *error-output-buffer*) "red")
                                               (cons (get-output-stream-string *terminal-out-buffer*) "black")))))
    (format nil "~A~%" (if (x:empty-string cmd) ":r1" cmd))))

