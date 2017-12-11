;;;
;;; Use of PaintedItem (a QQuickPaintedItem) and QPainter
;;; (meant as an alternative to Canvas in QML).
;;;

(qrequire :quick)

(in-package :painted-item)

(defun clock ()
  (find-quick-item "clock"))

;; paint

(defun qml:paint (item painter)
  "This function is called on every 'paint' request of QML type 'PaintedItem'."
  (|setRenderHint| painter |QPainter.Antialiasing|)
  (cond ((qeql (clock) item)
         (clock:paint item painter))))

(defun start ()
  (qml:ini-quick-view "qml/painted-item.qml")
  ;; update painting every second
  (let ((timer (qnew "QTimer")))
    (qconnect timer "timeout()" (lambda () (|update| (clock))))
    (|start| timer 1000)))
