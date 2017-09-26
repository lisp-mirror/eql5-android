;;; A skeleton for interactive development

(qrequire :quick)

(defpackage :ini-qml
  (:use :cl :eql :qml)
  (:export))

(in-package :ini-qml)

(defun ini-qml (file)
  (setf qml:*quick-view* (qnew "QQuickView"))
  ;; special settings for mobile, taken from Qt example
  (let ((env (ext:getenv "QT_QUICK_CORE_PROFILE")))
    (when (and (stringp env)
               (not (zerop (parse-integer env :junk-allowed t))))
      (let ((f (|format| *quick-view*)))
        (|setProfile| f |QSurfaceFormat.CoreProfile|)
        (|setVersion| f 4 4)
        (|setFormat| *quick-view* f))))
  (qconnect (|engine| *quick-view*) "quit()" (qapp) "quit()")
  (qnew "QQmlFileSelector(QQmlEngine*,QObject*)" (|engine| *quick-view*) *quick-view*)
  (|setSource| *quick-view* (file-to-url file))
  (when (= |QQuickView.Error| (|status| *quick-view*))
    ;; display eventual QML errors
    (qmsg (list (mapcar '|toString| (|errors| *quick-view*))))
    (return-from ini-qml))
  (|setResizeMode| *quick-view* |QQuickView.SizeRootObjectToView|)
  (let ((platform (|platformName.QGuiApplication|)))
    (if (find platform '("qnx" "eglfs") :test 'string=)
        (|showFullScreen| *quick-view*)
        (|show| *quick-view*))))

;; REPL

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
        (qml:reload)))
  (values))

(defun qml-reloaded ()
  "Ini after QML has been reloaded."
  ;; TODO
  )

;;; 

(defun start ()
  ;; ini
  (qlater 'eql-user::ini)
  (eval:ini)
  (ini-qml "qml/skeleton.qml")
  (qconnect qml:*quick-view* "statusChanged(QQuickView::Status)" ; for reloading
            (lambda (status)
              (when (= |QQuickView.Ready| status)
                (qml-reloaded)))))
