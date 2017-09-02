(in-package :editor)

(qrequire :quick)

(dolist (module (list :network :sql :svg))
  (qrequire module :quiet)) ; load if available

(defvar *max-history*         100)
(defvar *package-char-dummy*  #\$)
(defvar *separator*           "#||#")
(defvar *lisp-match-rule*     nil)
(defvar *eql-keyword-format*  nil)
(defvar *lisp-keyword-format* nil)
(defvar *comment-format*      nil)
(defvar *parenthesis-color*   "lightslategray")
(defvar *string-color*        "saddlebrown")
(defvar *highlighter*         nil)
(defvar *history-file*        nil)
(defvar *file*                nil)

;; QML items
(defvar *qml-edit*     "edit")
(defvar *qml-output*   "output")
(defvar *qml-status*   "status")
(defvar *qml-document* nil)

(defun read-file (file)
  (with-open-file (s (x:path file) :direction :input)
    (x:let-it (make-string (file-length s))
      (read-sequence x:it s))))

(defun connect-buttons ()
  (flet ((clicked (name function)
           (qconnect (find-quick-item name) "clicked()" function)))
    (clicked "open_file"    'open-file)
    (clicked "save_file"    'save-file)
    (clicked "clear"        'clear)
    (clicked "history_up"   (lambda () (history-move :up)))
    (clicked "history_down" (lambda () (history-move :down)))
    (clicked "eval"         'eval-expression)
    (clicked "font_bigger"  (lambda () (change-font :bigger)))
    (clicked "font_smaller" (lambda () (change-font :smaller)))))

(defun ini-highlighter ()
  (setf *eql-keyword-format*  (qnew "QTextCharFormat")
        *lisp-keyword-format* (qnew "QTextCharFormat")
        *comment-format*      (qnew "QTextCharFormat")
        *lisp-match-rule*     (qnew "QRegExp(QString)" "[(']:*[^ )]+"))
  (|setForeground| *eql-keyword-format*  (qnew "QBrush(QColor)" "#0000C0"))
  (|setForeground| *lisp-keyword-format* (qnew "QBrush(QColor)" "#C00000"))
  (x:do-with *comment-format*
    (|setForeground| (qnew "QBrush(QColor)" "#80A080"))
    (|setFontItalic| t))
  (setf *highlighter* (qnew "QSyntaxHighlighter(QTextDocument*)" *qml-document*))
  (qoverride *highlighter* "highlightBlock(QString)"
             (lambda (str) (highlight-block *highlighter* str))))

(defun read* (str &optional (start 0))
  (setf *try-read-error* nil)
  (let ((*package* #.(find-package :eql)))
    (multiple-value-bind (exp x)
        (ignore-errors (read-from-string (substitute *package-char-dummy* #\: str)
                                         nil nil :start start :preserve-whitespace t))
      (unless exp
        (setf *try-read-error* (typecase x
                                 (end-of-file :end-of-file)
                                 (t t))))
      (values exp x))))

(defun end-position (expr)
  (multiple-value-bind (x end)
      (read* expr)
    (when (numberp end)
      end)))

(let (qt-matches cache-matches)
  (flet ((qt-fun (pos)
           (cdr (assoc (- pos 2) qt-matches)))
         (qt-pos (fun)
           (car (find fun qt-matches :key 'cdr))))
    (defun highlight-block (highlighter text)
      (unless (x:empty-string text)
        (setf latest highlighter)
        (when cache-matches
          (setf qt-matches nil))
        (let ((i (|indexIn| *lisp-match-rule* text)))
          (x:while (>= i 0)
            (let* ((len (|matchedLength| *lisp-match-rule*))
                   (kw* (subseq text (1+ i) (+ i len)))
                   (kw (x:if-it (position #\: kw* :from-end t)
                           (subseq kw* (1+ x:it))
                           kw*)))
              (flet ((set-format (frm)
                       (|setFormat| highlighter (1+ i) (1- len) frm)))
                (cond ((find kw *eql-keywords* :test 'string=)
                       (when cache-matches
                         (push (cons (+ i len) (intern (string-upcase kw) :keyword))
                               qt-matches))
                       (set-format *eql-keyword-format*))
                      ((gethash kw *lisp-keywords*)
                       (set-format *lisp-keyword-format*))))
              (setf i (|indexIn| *lisp-match-rule* text (+ i len))))))
        (setf cache-matches nil)
        ;; comments, strings, parenthesis
        (flet ((set-color (pos len color)
                 (|setFormat| highlighter pos len color)))
          (let ((ex #\Space))
            (dotimes (i (length text))
              (let ((ch (char text i)))
                (unless (char= #\\ ex)
                  (case ch
                    ((#\( #\))
                     (set-color i 1 *parenthesis-color*))
                    (#\"
                     (x:when-it (end-position (subseq text i))
                       (set-color i x:it *string-color*)
                       (incf i (1- x:it))))
                    (#\;
                     (|setFormat| highlighter i (- (length text) i) *comment-format*)
                     (return))))
                (setf ex ch)))))))
    (defun cursor-position-changed (text-cursor)
      (setf cache-matches t)
      (let* ((text-block (|block| text-cursor))
             (line (|text| text-block))
             (pos (|positionInBlock| text-cursor)))
        (when (and (plusp pos)
                   (char= #\) (char line (1- pos))))
          (show-matching-paren text-cursor (subseq line 0 pos) :right))))))

;;; paren highlighting

(defun code-parens-only (code &optional right)
  "Substitute all non code related parenthesis with a space character."
  (let ((ex #\Space)
        (len (length code))
        comment in-string)
    (dotimes (i len)
      (let* ((i* (if right (- len i 1) i))
             (ch (char code i*)))
        (cond ((char= #\\ ex)
               (when (find ch "();\"")
                 (setf (char code i*) #\Space)))
              ((and (not in-string) (char= #\; ch))
               (setf comment t))
              ((char= #\Newline ch)
               (setf comment nil))
              ((char= #\" ch)
               (setf in-string (not in-string)))
              ((or comment in-string)
               (when (find ch "()")
                 (setf (char code i*) #\Space))))
        (setf ex ch))))
  code)

(defun paren-match-index (code &optional (n 0))
  (dotimes (i (length code))
    (let ((ch (char code i)))
      (case ch
        (#\( (incf n))
        (#\) (decf n))))
    (when (zerop n)
      (return-from paren-match-index i))))

(defun code-region (text-cursor curr-line &optional right)
  (let ((max (|blockCount| *qml-document*)))
    (with-output-to-string (s)
      (write-line (if right (nreverse curr-line) curr-line) s)
      (do* ((n (|blockNumber| text-cursor) (+ n (if right -1 1)))
            (text-block (funcall (if right '|previous| '|next|) (|block| text-cursor))
                        (funcall (if right '|previous| '|next|) text-block ))
            (text (|text| text-block) (|text| text-block)))
           ((or (if right (zerop n) (= n max))
                (x:empty-string (string-trim '(" ") text))))
        (write-line (if right (nreverse text) text) s)))))

(defun left-right-paren (right text-cursor curr-line &optional pos)
  (paren-match-index (code-parens-only (code-region text-cursor curr-line right) right)))

(defun right-paren (text-cursor curr-line)
  (unless (x:ends-with "\\)" curr-line)
    (left-right-paren :right text-cursor curr-line)))

(let ((ex-from -1))
  (defun show-matching-paren (text-cursor line type)
    (x:when-it (right-paren text-cursor line)
      (let* ((pos (|position| text-cursor))
             (from (- pos x:it 1))
             (color (qml-get *qml-edit* "selectionColor")))
        (when (/= from ex-from)
          (setf ex-from from)
          (qsingle-shot 500 (lambda () (setf ex-from -1)))
          (qml-set *qml-edit* "readOnly" t)
          (qml-set *qml-edit* "selectionColor" "gray")
          (qml-call *qml-edit* "select" from (1+ from))
          (qsleep 0.1)
          (qml-set *qml-edit* "readOnly" nil)
          (qml-set *qml-edit* "cursorPosition" pos)
          (qml-set *qml-edit* "selectionColor" color))))))

(defun eval* (text)
  (eval:feed-top-level text))

(defun eval-output (type text)
  (let ((text* (qescape text)))
    (qml-call *qml-output* "append"
              (format nil "<pre><font face='Droid Sans Mono' color='~A'>~A</font></pre>"
                      (case type
                        (:output "saddlebrown")
                        (:values "blue")
                        (:trace  "darkmagenta")
                        (:error  "red")
                        (t       "black"))
                      (if (eql :values type)
                          (x:string-substitute "<br>" *separator* text*)
                          (x:string-substitute "<br>" (string #\Newline) text*))))))

(defun eval-expression ()
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return) (qml-get *qml-edit* "text"))))
    (eval* text)
    (history-add text)
    (qml-call *qml-edit* "clear")))

(defun saved-history ()
  (let ((ex "")
        history)
    (when (probe-file *history-file*)
      (with-open-file (s *history-file* :direction :input)
        (x:while-it (read-line s nil nil)
          (when (string/= ex x:it)
            (setf ex x:it)
            (push (x:string-substitute (string #\Newline) *separator* x:it)
                  history))))
      (setf history (nthcdr (max 0 (- (length history) *max-history*)) (reverse history)))
      (with-open-file (s *history-file* :direction :output
                         :if-exists :overwrite)
        (dolist (cmd history)
          (write-line (x:string-substitute *separator* (string #\Newline) cmd)
                      s)))
      (reverse history))))

(let ((ex :up)
      down out)
  (defun history-ini ()
    (setf up  (saved-history)
          out (open *history-file* :direction :output
                    :if-exists :append :if-does-not-exist :create)))
  (defun history-move (direction)
    (setf *file* nil)
    (unless out
      (history-ini))
    (let (exp)
      (dotimes (n (if (eql direction ex) 1 2))
        (setf exp (case direction
                    (:up
                     (x:when-it (pop up)
                       (push x:it down)))
                    (:down
                     (x:when-it (pop down)
                       (push x:it up))))))
      (setf ex direction)
      (when exp
        (qml-set *qml-edit* "text" (first exp)))))
  (defun history-add (cmd)
    (unless out
      (history-ini))
    (when (or (not up)
              (and up (string/= cmd (first up))))
      (push cmd up)
      (princ (x:string-substitute *separator* (string #\Newline) cmd)
             out)
      (terpri out)
      (force-output out)
      (when (and down (string= cmd (first down)))
        (pop down))))
  (defun history ()
    (append (reverse up) down)))

(defun change-font (to)
  (let ((size (+ (qml-get *qml-edit* "font.pointSize")
                 (if (eql :bigger to) 2 -2))))
    (qml-set *qml-edit* "font.pointSize" size)
    (qml-set *qml-output* "font.pointSize" size)
    (qml-set *qml-status* "font.pointSize" size)))

(defun clear ()
  (qml-call *qml-edit* "clear")
  (setf *file* nil))

;; open file

(defun trim-file (name)
  (if (x:starts-with "file://" name)
      (subseq name #.(length "file://"))
      name))

(defun open-file ()
  (dialogs:get-file-name 'do-open-file))

(defun do-open-file (name)
  (let ((name* (trim-file name)))
    (unless (x:empty-string name*)
      (setf *file* name*)
      (if (x:starts-with "fas" (pathname-type name*))
          (eval* (format nil "(load ~S)" name*))
          (qml-set *qml-edit* "text" (read-file name*))))))

;; save-file

(defun save-to-file (file &optional (qml-item *qml-edit*) append)
  (with-open-file (s file :direction :output
                     :if-exists (if append :append :supersede)
                     :if-does-not-exist :create)
    (write-sequence (qml-get qml-item "text") s)))

(defun confirm-save-dialog (title text)
  (= |QMessageBox.Save|
     (|question.QMessageBox| nil title text
                             (logior |QMessageBox.Save| |QMessageBox.Cancel|))))

(defun save-file ()
  (let ((dialog t))
    (when (and *file*
               (confirm-save-dialog "Save?"
                                    (format nil "Save to opened file, overwriting it?<br><br>~S<br>" *file*)))
      (save-to-file *file*)
      (setf dialog nil))
    (when dialog
      (dialogs:get-file-name 'do-save-file :save))))

(defun do-save-file (name)
  (let ((file (trim-file name))
        (ok t))
    (unless (x:empty-string file)
      (when (probe-file file)
        (unless (confirm-save-dialog "Overwrite?"
                                     (format nil "File already exists; overwrite?<br><br>~S<br>" file))
          (setf ok nil)))
      (when ok
        (save-to-file file)))))

;; log

(defun log-output ()
  (save-to-file "output-log.htm" *qml-output* :append)
  t)

;; ini

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

(defun set-text-document () ; called from QML
  ;; needed because QML-GET can't return QObject* pointers
  (setf *qml-document* (|textDocument| qml:*caller*))
  (ini-highlighter)
  (qconnect *qml-document* "cursorPositionChanged(QTextCursor)" 'cursor-position-changed))

(defun set-delayed-focus () ; called from QML
  ;; needed because resizing sometimes gets messed up on startup
  ;; (caused by virtual keyboard)
  (qsingle-shot 1000 (lambda ()
                       (qml-call "clear" "forceActiveFocus")
                       (qlater (lambda ()
                                 (qml-call "edit" "forceActiveFocus"))))))

(defun start ()
  (ini-qml "qml/repl.qml")
  (connect-buttons)
  (qconnect qml:*quick-view* "statusChanged(QQuickView::Status)" ; for reloading
            (lambda (status)
              (when (= |QQuickView.Ready| status)
                (connect-buttons)
                (setf eql::*reloading-qml* nil))))
  (eval:ini :output       'eval-output
            :query-dialog 'dialogs:query-dialog
            :debug-dialog 'dialogs:debug-dialog)
  (setf *history-file* (x:cc (first (|standardLocations.QStandardPaths| |QStandardPaths.HomeLocation|))
                             "/.eql5-lisp-repl-history"))
  (setf *break-on-errors* t))

(defun reload-qml (&optional (url "http://localhost:8080/"))
  ;; please see README-1.md
  "Reload QML file from an url, directly on the device."
  (setf eql::*reloading-qml* t)
  (let ((src (|toString| (|source| qml:*quick-view*))))
    (if (x:starts-with "qrc:/" src)
        (|setSource| qml:*quick-view* (qnew "QUrl(QString)"
                                            (x:string-substitute url "qrc:/" src)))
        (qml:reload)))
  ;; don't remove message box (won't work without / event queue problem)
  (|information.QMessageBox| nil "REPL" "<b>QML</b> files reloaded.")
  :qml-reloaded)
