(qrequire :quick)

(in-package :sensors)

(defun start ()
  ;; ini
  (qlater 'eql-user::ini) ; for Swank, Quicklisp
  (eval:ini)
  ;; QML ini
  (qml:ini-quick-view "qml/sensors.qml")
  (ini)
  (qconnect qml:*quick-view* "statusChanged(QQuickView::Status)" ; for reloading
            (lambda (status)
              (case status
                (#.|QQuickView.Ready|
                 (qml-reloaded))
                (#.|QQuickView.Error|
                 (qmsg (x:join (mapcar '|toString| (|errors| *quick-view*))
                               #.(make-string 2 :initial-element #\Newline)))))))
  ;; show help
  (qlater (lambda () (eval:eval-in-thread "(help)"))))

;;; REPL

(defvar *qml-repl*  "repl_container")
(defvar *qml-accel* "accel")

(defun show-repl (show) ; called from QML
  (when show
    (qml-set *qml-repl* "opacity" 0)
    (qml-set *qml-repl* "visible" t))
  (dotimes (n 10)
    (qml-set *qml-repl* "opacity" (/ (if show (1+ n) (- 9 n)) 10))
    (qsleep 0.015))
  (unless show
    (qml-set *qml-repl* "visible" nil))
  (qml-set *qml-accel* "active" (not show))) ; would interfere with REPL

(defun reload-qml (&optional (url "http://localhost:8080/"))
  ;; please see README-1.md in REPL example
  "Reload QML file from an url, directly on the device."
  (let ((src (|toString| (|source| qml:*quick-view*))))
    (if (x:starts-with "qrc:/" src)
        (|setSource| qml:*quick-view* (qnew "QUrl(QString)"
                                            (x:string-substitute url "qrc:/" src)))
        (qml:reload))
    (|toString| (|source| qml:*quick-view*))))

(defun qml-reloaded ()
  (ini))
