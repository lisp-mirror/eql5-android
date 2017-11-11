;;; This is a port of QML example "accelbubble"

(in-package :sensors)

(defvar *qml-bubble* nil)

(defvar *max-x* nil)
(defvar *max-y* nil)

(defconstant +const+ 57.2957795)

;; we need to be fast here, so we avoid QML-GET wherever possible,
;; using only QML-SET (for eventually attached animations etc.)

(defun ini ()
  (setf *qml-bubble* (find-quick-item "bubble"))
  (let ((qml-main (find-quick-item "main")))
    (setf *max-x* (- (|width|  qml-main) (|width|  *qml-bubble*))
          *max-y* (- (|height| qml-main) (|height| *qml-bubble*)))))

(defun move-bubble (x y z)
  (flet ((pitch ()
           (- (* +const+ (atan (/ y (sqrt (+ (* x x) (* z z))))))))
         (roll ()
           (- (* +const+ (atan (/ x (sqrt (+ (* y y) (* z z)))))))))
    (let ((new-x (+ (|x| *qml-bubble*) (/ (roll) 10)))
          (new-y (- (|y| *qml-bubble*) (/ (pitch) 10))))
      (qml-set *qml-bubble* "x" (max 0 (min new-x *max-x*)))
      (qml-set *qml-bubble* "y" (max 0 (min new-y *max-y*))))))
