;;; This is a port of QML example "accelbubble"

(in-package :sensors)

(defvar *max-x* nil)
(defvar *max-y* nil)

(defvar *qml-main*    "main")
(defvar *qml-accel*   "accel")
(defvar *qml-compass* "compass")
(defvar *qml-bubble*  "bubble")
(defvar *qml-azimuth* "azimuth")

(si::trap-fpe t nil) ; ignore floating point overflows

;; we use a single shot timer for the sensor read interval, so we are independent of
;; both the data rate of the device (which could be very high, a few hundret Hertz)
;; and the data processing speed (which could be slow in some cases)

(defun ini ()
  (setf *max-x* (- (qml-get *qml-main* "width")  (qml-get *qml-bubble* "width"))
        *max-y* (- (qml-get *qml-main* "height") (qml-get *qml-bubble* "height")))
  (when (qml-get *qml-accel* "active")   ; don't start if sensor isn't present
    (move-bubble))
  (when (qml-get *qml-compass* "active") ; don't start if sensor isn't present
    (display-azimuth)))

;;; accelerometer

(defvar *accel-timer* nil)

(defun move-bubble ()
  (unless *accel-timer*
    (setf *accel-timer* (qnew "QTimer"
                              "interval" 10
                              "singleShot" t))
    (qconnect *accel-timer* "timeout()" 'move-bubble))
  (do-move-bubble)
  (|start| *accel-timer*))

(defconstant +const+ 57.2957795)

(defun do-move-bubble ()
  (x:when-it (find-quick-item *qml-accel*) ; needed for QML reloading
    (let ((x (qml-get x:it "reading.x"))
          (y (qml-get x:it "reading.y"))
          (z (qml-get x:it "reading.z")))
      (flet ((pitch ()
               (- (* +const+ (atan (/ y (sqrt (+ (* x x) (* z z))))))))
             (roll ()
               (- (* +const+ (atan (/ x (sqrt (+ (* y y) (* z z)))))))))
        (let ((new-x (+ (qml-get *qml-bubble* "x") (/ (roll) 10)))
              (new-y (- (qml-get *qml-bubble* "y") (/ (pitch) 10))))
          (qml-set *qml-bubble* "x" (max 0 (min new-x *max-x*)))
          (qml-set *qml-bubble* "y" (max 0 (min new-y *max-y*))))))))

;;; compass

(defvar *compass-timer* nil)

(defun round* (x)
  (if x (truncate (+ 0.5 x)) 0))

(defun display-azimuth ()
  (unless *compass-timer*
    (setf *compass-timer* (qnew "QTimer"
                                "interval" 500
                                "singleShot" t))
    (qconnect *compass-timer* "timeout()" 'display-azimuth))
  (qml-set *qml-azimuth* "text"
           (princ-to-string (round* (qml-get *qml-compass* "reading.azimuth"))))
  (|start| *compass-timer*))

;;; interrupt timers

(defun %timers (fun)
  (when *accel-timer*
    (funcall fun *accel-timer*))
  (when *compass-timer*
    (funcall fun *compass-timer*)))

(defun start-sensor-timers ()
  (%timers '|start|))

(defun stop-sensor-timers ()
  (%timers '|stop|))

