(qrequire :quick)

(in-package :my)

(defun start ()
  ;; ini
  (qlater 'eql-user::ini) ; for Swank, Quicklisp
  (eval:ini)
  (setf eql:*break-on-errors* t)
  ;; QML ini
  (qml:ini-quick-view "qml/my.qml")
  (qconnect qml:*quick-view* "statusChanged(QQuickView::Status)" ; for reloading
            (lambda (status)
              (case status
                (#.|QQuickView.Ready|
                 (qml-reloaded))
                (#.|QQuickView.Error|
                 (qmsg (list (mapcar '|toString| (|errors| *quick-view*))))))))
  ;; show help
  (qlater (lambda () (eval:eval-in-thread "(help)"))))

;;; REPL

(defvar *qml-repl* "repl_container")

(defun show-repl (show) ; called from QML
  (when show
    (qml-set *qml-repl* "opacity" 0)
    (qml-set *qml-repl* "visible" t))
  (dotimes (n 10)
    (qml-set *qml-repl* "opacity" (/ (if show (1+ n) (- 9 n)) 10))
    (qsleep 0.015))
  (unless show
    (qml-set *qml-repl* "visible" nil)))

(defun reload-qml (&optional (url "http://localhost:8080/"))
  ;; please see README-1.md in REPL example
  "Reload QML file from an url, directly on the device."
  (let ((src (|toString| (|source| qml:*quick-view*))))
    (if (x:starts-with "qrc:/" src)
        (|setSource| qml:*quick-view* (qnew "QUrl(QString)"
                                            (x:string-substitute url "qrc:/" src)))
        (qml:reload))
    src))

(defun qml-reloaded ()
  ;; re-ini
  )
