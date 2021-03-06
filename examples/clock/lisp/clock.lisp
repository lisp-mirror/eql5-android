(defpackage :clock
  (:use :cl :eql)
  (:export
   #:paint))

(in-package :clock)

(defun pen (width color)
  (x:let-it (qnew "QPen")
    (x:do-with x:it
      (|setCapStyle| |Qt.RoundCap|)
      (|setWidth| width)
      (|setColor| color))))

(defun brush (color)
  (x:let-it (qnew "QBrush")
    (x:do-with x:it
      (|setStyle| |Qt.SolidPattern|)
      (|setColor| color))))

(defparameter *pen-clock*      (pen 1  "white"))
(defparameter *brush-clock*    (brush  "white"))
(defparameter *pen-hour-marks* (pen 4  "black"))
(defparameter *pen-hour*       (pen 10 "red"))
(defparameter *pen-minute*     (pen 8  "blue"))

(defun paint (item p)
  (macrolet ((with-save (() &body body)
               `(progn (|save| p) ,@body (|restore| p))))
    (let* ((size (nthcdr 2 (|contentsBoundingRect| item)))
           (scale (/ (apply 'min size) 170)))
      (|translate| p (mapcar (lambda (x) (/ x 2)) size))
      (|scale| p scale scale))
    (|rotate| p -90)
    (|setPen| p *pen-clock*)
    (|setBrush| p *brush-clock*)
    (|drawEllipse| p '(-75 -75 150 150))
    (|setPen| p *pen-hour-marks*)
    (dotimes (n 12)
      (|rotate| p 30)
      (|drawLine| p '(55 0 65 0)))
    (multiple-value-bind (sec min hour)
        (get-decoded-time)
      (|setOpacity| p 0.5)
      (with-save ()
        (|rotate| p (+ (* 30 hour) (/ min 2)))
        (|setPen| p *pen-hour*)
        (|drawLine| p '(0 0 35 0)))
      (with-save ()
        (|rotate| p (* 6 min))
        (|setPen| p *pen-minute*)
        (|drawLine| p '(0 0 64 0))))))

;; quit app

(defun eql::back-pressed () ; called from QML
  (qquit))
