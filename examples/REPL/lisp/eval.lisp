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
                                                     *standard-output-buffer*
                                                     *status-standard-buffer*)
            *trace-output*    (make-broadcast-stream *trace-output*
                                                     *trace-output-buffer*
                                                     *status-trace-buffer*)
            *error-output*    (make-broadcast-stream *error-output*
                                                     *error-output-buffer*
                                                     *status-error-buffer*)))
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
      (multiple-value-bind (x end)
          (ignore-errors (read-from-string str))
        ;; use LOAD if there is more than 1 form
        (setf si::*read-string*
              (if (eql (length str) end)
                  str
                  (format nil "(load (make-string-input-stream ~S))" str))))
      ;; run eval in its own thread, so GUI will remain responsive
      ;; N.B. this is only safe because we use "thread-safe.lisp" (like in Slime mode)
      (unless *log-mode*
        (clear-status-buffers)
        (start-status-timer))
      (qml:qml-set "status_bar" "visible" t)
      (setf *eval-thread* (mp:process-run-function "EQL5 REPL top-level" 'start-top-level)))))

(defun set-eval-state (evaluating)
  (qml:qml-set "eval" "enabled" (not evaluating))
  (qml:qml-set "eval" "text" (if evaluating
                                 "<font color='red'><b>Evaluating</b></font>"
                                 "<b>Eval</b>")))

(defun start-top-level ()
  (set-eval-state t)
  (setf *debug-invoked* nil
        *query-invoked* nil)
  (write-output :expression *standard-output-buffer*)
  (si::%top-level)
  (unless *log-mode*
    (stop-status-timer))
  (qml:qml-set "status_bar" "visible" nil)
  (qml:qml-set "status" "text" "")
  (write-output :error  *error-output-buffer*)
  (write-output :trace  *trace-output-buffer*)
  (write-output :output *standard-output-buffer*)
  (when (and *gui-output*
             (or (not *debug-invoked*)
                 (and *debug-invoked*
                      *query-invoked*)))
    (funcall *gui-output* :values (format nil "~{~S~^#||#~}" si::*latest-values*))) ; "#||#": separator
  (set-eval-state nil))

(defun handle-query-io ()
  (setf *query-invoked* t)
  (let ((text (funcall *gui-query-dialog* (get-output-stream-string *terminal-out-buffer*))))
    (when (and *gui-output*
               (not (x:empty-string text)))
      (funcall *gui-output* :values text))
    (format nil "~A~%" text)))

(defun handle-debug-io ()
  (set-eval-state nil)
  (setf *debug-invoked* t)
  (let ((cmd (funcall *gui-debug-dialog* (list (cons (get-output-stream-string *error-output-buffer*) "red")
                                               (cons (get-output-stream-string *terminal-out-buffer*) "black")))))
    (format nil "~A~%" (if (x:empty-string cmd) ":r1" cmd))))

;; status bar & log mode

(defvar *status-timer*           nil)
(defvar *status-standard-buffer* (make-string-output-stream))
(defvar *status-trace-buffer*    (make-string-output-stream))
(defvar *status-error-buffer*    (make-string-output-stream))
(defvar *status-standard-line*   "")
(defvar *status-trace-line*      "")
(defvar *status-error-line*      "")

(defvar *log-mode*               nil)
(defvar *log-stream*             nil)
(defvar *log-file*               "logs/output.txt")

(defun start-status-timer (&optional (interval 500))
  (unless *status-timer*
    (setf *status-timer* (qnew "QTimer"))
    (qconnect *status-timer* "timeout()" 'update-status))
  (|start| *status-timer* interval))

(defun stop-status-timer ()
  (|stop| *status-timer*))

(defun start-logging ()
  (ensure-directories-exist *log-file*)
  (setf *log-stream* (open *log-file* :direction :output :if-exists :supersede))
  (|stop| *status-timer*)
  (|start| *status-timer* 100)
  (setf *log-mode* t))

(defun stop-logging ()
  (when *log-stream*
    (close *log-stream*)
    (|stop| *status-timer*)
    (setf *log-stream* nil
          *log-mode*   nil)))

(defun display-log ()
  (flet ((read-log ()
           (with-open-file (s *log-file* :direction :input)
             (x:let-it (make-string (file-length s))
               (read-sequence x:it s)))))
    (qml:qml-set "output" "text"
                 (x:string-substitute "<br>" (string #\Newline) (qescape (read-log))))))

(defun clear-status-buffers ()
  (get-output-stream-string *status-standard-buffer*)
  (get-output-stream-string *status-trace-buffer*)
  (get-output-stream-string *status-error-buffer*))

(defun update-status-line (stream line-var)
  (let ((chunk (get-output-stream-string stream)))
    (unless (x:empty-string chunk)
      (if *log-mode*
          (progn
            (write-string chunk *log-stream*)
            (finish-output *log-stream*))
          (let* ((line (x:cc (symbol-value line-var) chunk))
                 (nl (position #\Newline line :from-end t)))
            (setf (symbol-value line-var)
                  (if nl (subseq line (1+ nl)) line))))
      t)))

(defun update-status ()
  (mapc (lambda (stream line-var)
          (when (update-status-line stream line-var)
            (unless *log-mode*
              (qml:qml-set "status" "text" (symbol-value line-var)))
            (return-from update-status)))
          (list *status-standard-buffer* *status-trace-buffer* *status-error-buffer*)
          '(*status-standard-line* *status-trace-line* *status-error-line*))
  (when (and (not *log-mode*)
             (x:empty-string (qml:qml-get "status" "text")))
    (qml:qml-set "status" "text" "Evaluating...")))
