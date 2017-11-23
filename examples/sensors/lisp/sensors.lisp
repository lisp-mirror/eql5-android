;;; This is an extended port of QML example "accelbubble"

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

(defvar *accel-timer*   nil)
(defvar *accel-reading* nil)

(defun set-accel-reading () ; called from QML
  (setf *accel-reading* qml:*caller*))

(defun move-bubble ()
  (unless *accel-timer*
    (setf *accel-timer* (qnew "QTimer"
                              "interval" 1     ; for max. speed
                              "singleShot" t))
    (qconnect *accel-timer* "timeout()" 'move-bubble))
  (do-move-bubble)
  (|start| *accel-timer*))

(defconstant +const+ (/ 180 pi))

(defun do-move-bubble ()
  ;; this function is speed optimized, using neither QML-GET nor QML-SET;
  ;; for this reason you can't have e.g. animations attached to the 'bubble' item;
  ;; if you need animations etc. on the 'bubble' item, use the (slower) QML-SET
  (when (find-quick-item *qml-accel*)     ; needed for QML reloading
    (let ((x (qget *accel-reading* "x"))  ; fastest way to read sensor data
          (y (qget *accel-reading* "y"))
          (z (qget *accel-reading* "z")))
      (flet ((pitch ()
               (- (* +const+ (atan (/ y (sqrt (+ (* x x) (* z z))))))))
             (roll ()
               (- (* +const+ (atan (/ x (sqrt (+ (* y y) (* z z)))))))))
        (let* ((bubble (find-quick-item *qml-bubble*))
               (new-x (+ (|x| bubble) (/ (roll) 10)))
               (new-y (- (|y| bubble) (/ (pitch) 10))))
          (|setX| bubble (max 0 (min new-x *max-x*)))
          (|setY| bubble (max 0 (min new-y *max-y*))))))))

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
  ;; not speed optimized (long interval)
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
