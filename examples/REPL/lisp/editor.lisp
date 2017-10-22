;;; This is a simplified / adapted version of EQL example 9

(in-package :editor)

(qrequire :quick)

(dolist (module (list :network :sql :svg))
  (qrequire module :quiet)) ; load if available

(defvar *package-char-dummy*     #\$)
(defvar *separator*              "#||#")
(defvar *lisp-match-rule*        nil)
(defvar *eql-keyword-format*     nil)
(defvar *lisp-keyword-format*    nil)
(defvar *comment-format*         nil)
(defvar *parenthesis-color*      "lightslategray")
(defvar *string-color*           "saddlebrown")
(defvar *current-line*           "")
(defvar *current-depth*          0)
(defvar *current-keyword-indent* 0)
(defvar *highlighter*            nil)
(defvar *file*                   nil)

;; QML items
(defvar *qml-edit*           "edit")
(defvar *qml-flick-edit*     "flick_edit")
(defvar *qml-command*        "command")
(defvar *qml-output*         "output")
(defvar *qml-clear*          "clear")
(defvar *qml-status*         "status")
(defvar *qml-clipboard-menu* "clipboard_menu")
(defvar *qml-document*        nil)

(defun read-file (file)
  (with-open-file (s (x:path file) :direction :input)
    (x:let-it (make-string (file-length s))
      (read-sequence x:it s))))

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

(defun end-position (str &optional simple)
  (multiple-value-bind (x end)
      (if simple
          (ignore-errors (read-from-string (code-parens-only str)
                                           nil nil :preserve-whitespace t))
          (read* str))
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
        (setf *current-line*  line
              *cursor-indent* pos)
        (when (and (plusp pos)
                   (char= #\) (char line (1- pos))))
          (show-matching-paren text-cursor (subseq line 0 pos) :right))))))

;;; auto-indent

(defparameter *two-spaces-indent-symbols*
 '(case ccase ecase ctypecase etypecase handler-bind handler-case catch
   defstruct defun defmacro destructuring-bind do do* dolist dotimes
   do-all-symbols do-symbols flet labels lambda let let* loop
   multiple-value-bind prog progn prog1 prog2 qlet typecase unless when
   with-open-file with-output-to-string eql::do-string eql::do-with
   eql::let-it eql::when-it eql::when-it* eql::while eql::while-it))

(defun auto-indent-spaces (kw)
  (when (symbolp kw)
    (let* ((name (symbol-name kw))
           (p (x:if-it (position *package-char-dummy* name :from-end t)
                       (1+ x:it)
                       0)))
      (when (find (read* (subseq name p)) *two-spaces-indent-symbols*)
        2))))

(defun cut-comment (line)
  (let ((ex #\Space))
    (dotimes (i (length line))
      (let ((ch (char line i)))
        (when (and (char= #\; ch)
                   (char/= #\\ ex))
          (return-from cut-comment (subseq line 0 i)))
        (setf ex ch))))
  line)

(defun last-expression-indent (line)
  (let* ((line* (string-right-trim " " (x:string-substitute "  " "\\(" (x:string-substitute "  " "\\)" (cut-comment line)))))
         (open  (position #\( line* :from-end t))
         (space (when open (position #\Space line* :start open)))
         (one   (and open (not space) (not (x:ends-with ")" line*)))))
    (if one
        (1+ open)
        (or (position #\Space (if space line* line) :test 'char/= :start (or space 0))
            0))))

(defun update-indentations (code indent pos)
  (flet ((pos-newline (start)
           (when start
             (or (position #\Newline code :start start) (length code)))))
    (let* ((pos-keyword    (paren-match-index code -1))
           (pos-local      (paren-match-index code -3))
           (keyword-indent (x:when-it (pos-newline pos-keyword) (- x:it pos-keyword 1)))
           (auto-indent    (auto-indent-spaces (read* (reverse (subseq code 0 pos-keyword)))))
           (in-local       (find (read* (reverse (subseq code 0 pos-local))) '(flet labels macrolet)))
           (local-indent   (x:when-it (and in-local (pos-newline pos-local)) (- x:it pos-local 1))))
      (setf *current-depth*          (or local-indent (if auto-indent (or keyword-indent pos) pos))
            *current-keyword-indent* (if local-indent
                                         (+ 5 (length (symbol-name in-local)))
                                         (or auto-indent 0))))))

(defun indentation (line)
  (if (x:empty-string (string-trim " " line))
      0
      (+ *current-depth* *current-keyword-indent*)))

(defun return-pressed () ; called from QML
  (let ((spaces (indentation *current-line*)))
    (unless (zerop spaces)
      (qlater (lambda ()
                (qml-call *qml-edit* "insert"
                          (qml-get *qml-edit* "cursorPosition")
                          (make-string spaces)))))))

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
  (let ((code (code-parens-only (code-region text-cursor curr-line right) right)))
    (x:when-it (paren-match-index code)
      (when right
        (update-indentations code x:it (- (position #\Newline code :start x:it) x:it 1)))
      x:it)))

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
          (let ((content-y (qml-get *qml-flick-edit* "contentY")))
            (qml-set *qml-edit* "readOnly" t)
            (qml-set *qml-edit* "selectionColor" "gray")
            (qml-call *qml-edit* "select" from (1+ from))
            (qsleep 0.1)
            (qml-set *qml-edit* "readOnly" nil)
            (qml-set *qml-edit* "cursorPosition" pos)
            (qml-set *qml-edit* "selectionColor" color)
            (qml-set *qml-flick-edit* "contentY" content-y)))))))

(defun eval* (text)
  (eval:feed-top-level text))

(defun eval-output (type text)
  (let ((text* (qescape text)))
    ;; "insert" is cleaner with formatting than "append"
    (qml-call *qml-output* "insert"
              (qml-get *qml-output* "length")
              (format nil "<pre><font face='~A' color='~A'>~%~A</font></pre>"
                      #+android "Droid Sans Mono"
                      #-android "Monospace"
                      (case type
                        (:output "saddlebrown")
                        (:values "blue")
                        (:trace  "darkmagenta")
                        (:error  "red")
                        (t       "black"))
                      (if (eql :values type)
                          (x:string-substitute "<br>" *separator* text*)
                          (x:string-substitute "<br>" (string #\Newline) text*)))))
  (qml-set *qml-output* "cursorPosition"
           (qml-get *qml-output* "length")))

(defun eval-expression (&optional single (history t))
  (let ((text (string-trim '(#\Space #\Tab #\Newline #\Return)
                           (or single (qml-get *qml-edit* "text")))))
    (eval* text)
    (when (and single history)
      (history-add text))))

;;; command history

(defvar *history*       (make-array 0 :adjustable t :fill-pointer t))
(defvar *history-index* nil)
(defvar *history-file*  ".eql5-lisp-repl-history")
(defvar *max-history*   100)

(defun read-saved-history ()
  (when (probe-file *history-file*)
    (let ((i -1))
      (labels ((index ()
                 (mod i *max-history*))
               (next-index ()
                 (incf i)
                 (index)))
        (let ((tmp (make-array *max-history*))) ; ring buffer
          (with-open-file (s *history-file* :direction :input)
            (x:while-it (read-line s nil nil)
              (setf (svref tmp (next-index)) x:it)))
          (let ((max (min (1+ i) *max-history*)))
            (when (< max *max-history*)
              (setf i -1))
            (dotimes (n max)
              (vector-push-extend (svref tmp (next-index))
                                  *history*))
            (setf *history-index* (length *history*)))))))) ; 1 after last

(let (out)
  (defun history-ini ()
    (read-saved-history)
    (setf out (open *history-file* :direction :output
                    :if-exists :append :if-does-not-exist :create)))
  (defun history-add (line)
    (unless out
      (history-ini))
    (let ((len (length *history*)))
      (when (or (zerop len)
                (string/= line (aref *history* (1- len))))
        (vector-push-extend line *history*)
        (write-line line out)
        (force-output out)))
    (setf *history-index* (length *history*))) ; 1 after last
  (defun history-move (dir)
    (unless out
      (history-ini))
    (when *history-index*
      (setf *history-index* (if (eql :back dir)
                                (max (1- *history-index*) 0)
                                (min (1+ *history-index*) (1- (length *history*)))))
      (qml-set *qml-command* "text" (aref *history* *history-index*)))))

;;; etc.

(defun change-font (to)
  (let ((size (+ (qml-get *qml-edit* "font.pixelSize")
                 (if (eql :bigger to) 3 -3))))
    (dolist (item (list *qml-edit* *qml-command* *qml-output* *qml-status*))
      (qml-set item "font.pixelSize" size))))

(defun clear ()
  (qml-call *qml-edit* "clear")
  (setf *file* nil))

;;; open file

(defun open-file ()
  (confirm-save-changes)
  (dialogs:get-file-name 'do-open-file))

(defun do-open-file ()
  (unless (x:empty-string dialogs:*file-name*)
    (setf *file* dialogs:*file-name*)
    (if (x:starts-with "fas" (pathname-type *file*))
        (eval* (format nil "(load ~S)" *file*))
        (qml-set *qml-edit* "text" (read-file *file*)))))

;;; save-file

(defun save-to-file (file &optional (qml-item *qml-edit*) append)
  (with-open-file (s file :direction :output
                     :if-exists (if append :append :supersede)
                     :if-does-not-exist :create)
    (write-sequence (qml-get qml-item "text") s)
    (|clearUndoRedoStacks| *qml-document*)))

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
      (dialogs:get-file-name 'do-save-file))))

(defun do-save-file ()
  (let ((ok t))
    (unless (x:empty-string dialogs:*file-name*)
      (when (probe-file dialogs:*file-name*)
        (unless (confirm-save-dialog "Overwrite?"
                                     (format nil "File already exists; overwrite?<br><br>~S<br>" dialogs:*file-name*))
          (setf ok nil)))
      (when ok
        (setf *file* dialogs:*file-name*)
        (save-to-file *file*)))))

(defun confirm-save-changes ()
  (when (and *file*
             (qml-get *qml-edit* "canUndo")
             (confirm-save-dialog "Save changes?"
                                  (format nil "Save changes to current file?<br><br>~S<br>" *file*)))
    (save-to-file *file*)))

;;; select all, cut, copy, paste

(defvar *selected-text*      "")
(defvar *copied-text*        "")
(defvar *selection-start*    0)
(defvar *cursor-indent-copy* 0)

(defun copy-paste (pos) ; called from QML
  (select-expression pos)
  (qml-call *qml-clipboard-menu* "open"))

(defun select-expression (pos)
  (let ((text (qml-get *qml-edit* "text"))
        (start pos)
        ch)
    (when (< pos (length text))
      (x:while (char/= #\( (setf ch (char text start)))
        (when (or (minusp (decf start))
                  (find ch '(#\Newline #\) )))
          (return-from select-expression)))
      (x:when-it (end-position (subseq text start) :simple)
        (let ((end (+ start x:it)))
          (setf *selection-start* start
                *selected-text*   (subseq text start end))
          (qml-call *qml-edit* "select" start end))))))

(defun select-all ()
  (setf *selection-start* nil)
  (qml-call *qml-edit* "selectAll"))

(defun cut ()
  (copy)
  (qml-call *qml-edit* "remove"
            *selection-start*
            (+ *selection-start* (length *copied-text*))))

(defun copy ()
  (if *selection-start*
      (progn
        (setf *copied-text* *selected-text*)
        (let* ((snip (qml-call *qml-edit* "getText" (max 0 (- *selection-start* 100)) *selection-start*))
               (nl (position #\Newline snip :from-end t)))
          (setf *cursor-indent-copy* (if nl (- (length snip) (1+ nl)) 0))))
      (setf *copied-text*        (qml-get *qml-edit* "text")
            *selection-start*    0
            *cursor-indent-copy* 0)))

(defun paste ()
  "Paste text adapting the indentation."
  (let* ((lines (x:split *copied-text* #\Newline))
         (diff (- *cursor-indent* *cursor-indent-copy*))
         (text (with-output-to-string (s)
                 (write-line (first lines) s)
                 (dolist (line (rest lines))
                   (when (plusp diff)
                     (write-string (make-string diff) s))
                   (write-line (subseq line (if (minusp diff) (- diff) 0)) s)))))
    (qml-call *qml-edit* "insert"
              (qml-get *qml-edit* "cursorPosition")
              (subseq text 0 (1- (length text))))))

(defun connect-menu-buttons ()
  (flet ((clicked (name function &optional (hide t))
           (qconnect (find-quick-item name) "clicked()"
                     (lambda ()
                       (funcall function)
                       (when hide
                         (qml-call *qml-clipboard-menu* "close"))))))
    (clicked "select_all" 'select-all nil)
    (clicked "cut"        'cut)
    (clicked "copy"       'copy)
    (clicked "paste"      'paste)
    (clicked "eval_exp"   (lambda () (eval-expression *selected-text* nil)))))

;;; log

(defun log-output ()
  (save-to-file "output-log.htm" *qml-output* :append)
  t)

;;; ini

(defun set-text-document () ; called from QML
  ;; needed because QML-GET can't return QObject* pointers
  (setf *qml-document* (|textDocument| qml:*caller*))
  (ini-highlighter)
  (qconnect *qml-document* "cursorPositionChanged(QTextCursor)" 'cursor-position-changed))

(defun set-delayed-focus () ; called from QML
  ;; needed because resizing sometimes gets messed up on startup
  ;; (caused by virtual keyboard)
  (qsingle-shot 1000 (lambda ()
                       (when (= (+ (qml-get *qml-edit* "height")
                                   (qml-get *qml-output* "height"))
                                (fourth (|availableGeometry| (|desktop.QApplication|))))
                         (|hide| (|inputMethod.QGuiApplication|))
                         (qprocess-events)
                         (qml-call *qml-edit* "forceActiveFocus")
                         (qprocess-events)
                         (qml-call *qml-command* "forceActiveFocus")))))

(defun connect-buttons ()
  (flet ((clicked (name function)
           (qconnect (find-quick-item name) "clicked()" function)))
    (clicked "open_file"       'open-file)
    (clicked "save_file"       'save-file)
    (clicked "clear"           'clear)
    (clicked "history_back"    (lambda () (history-move :back)))
    (clicked "history_forward" (lambda () (history-move :forward)))
    (clicked "eval"            'eval-expression)
    (clicked "font_bigger"     (lambda () (change-font :bigger)))
    (clicked "font_smaller"    (lambda () (change-font :smaller)))))

(defun start ()
  (qlater 'eql-user::ini) ; for Swank, Quicklisp
  (qml:ini-quick-view "qml/repl.qml")
  (connect-buttons)
  (connect-menu-buttons)
  (qconnect qml:*quick-view* "statusChanged(QQuickView::Status)" ; for reloading
            (lambda (status)
              (case status
                (#.|QQuickView.Ready|
                 (qml-reloaded))
                (#.|QQuickView.Error|
                 (qmsg (x:join (mapcar '|toString| (|errors| *quick-view*))
                               #.(make-string 2 :initial-element #\Newline)))))))
  (eval:ini :output       'eval-output
            :query-dialog 'dialogs:query-dialog
            :debug-dialog 'dialogs:debug-dialog)
  (setf *break-on-errors* t))

(defun reload-qml (&optional (url "http://localhost:8080/"))
  ;; please see README-1.md
  "Reload QML file from an url, directly on the device."
  (setf eql::*reloading-qml* t)
  (let ((src (|toString| (|source| qml:*quick-view*))))
    (if (x:starts-with "qrc:/" src)
        (|setSource| qml:*quick-view* (qnew "QUrl(QString)"
                                            (x:string-substitute url "qrc:/" src)))
        (qml:reload))
    (|toString| (|source| qml:*quick-view*))))

(defun qml-reloaded ()
  (connect-buttons)
  (connect-menu-buttons)
  (setf eql::*reloading-qml* nil))
