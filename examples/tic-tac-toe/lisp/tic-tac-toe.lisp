;;;
;;; for (c) please see COPYING.txt
;;;
;;; This is a port of a QtQuick1/Qt4 example.
;;; The JS game logic has been ported to Lisp.
;;;

(qrequire :quick)

(defun file-to-url (file)
  "Convert FILE to a QUrl, distinguishing between development and release version."
  #+release
  (qnew "QUrl(QString)" (x:cc "qrc:///" file)) ; see "Qt Resource System"
  #-release
  (|fromLocalFile.QUrl| file))

(defun run ()
  ;; special settings for mobile, taken from Qt example
  (setf qml:*quick-view* (qnew "QQuickView"))
  (let ((env (ext:getenv"QT_QUICK_CORE_PROFILE")))
    (when (and (stringp env)
               (not (zerop (parse-integer env :junk-allowed t))))
      (let ((f (|format| qml:*quick-view*)))
        (|setProfile| f |QSurfaceFormat.CoreProfile|)
        (|setVersion| f 4 4)
        (|setFormat| qml:*quick-view* f))))
  (qconnect (|engine| qml:*quick-view*) "quit()" (qapp) "quit()")
  (qnew "QQmlFileSelector(QQmlEngine*,QObject*)" (|engine| qml:*quick-view*) qml:*quick-view*)
  (|setSource| qml:*quick-view* (file-to-url "qml/tic-tac-toe.qml"))
  (when (= |QQuickView.Error| (|status| qml:*quick-view*))
    ;; display eventual QML errors
    (qmsg (list (mapcar '|toString| (|errors| qml:*quick-view*))))
    (return-from run))
  (|setResizeMode| qml:*quick-view* |QQuickView.SizeRootObjectToView|)
  (let ((platform (|platformName.QGuiApplication|)))
    (if (find platform '("qnx" "eglfs") :test 'string=)
        (|showFullScreen| qml:*quick-view*)
        (|show| qml:*quick-view*))))

(run)
