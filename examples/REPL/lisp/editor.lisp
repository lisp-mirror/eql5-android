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
(defvar *current-depth*          0)
(defvar *current-keyword-indent* 0)
(defvar *highlighter-edit*       nil)
(defvar *highlighter-command*    nil)
(defvar *file*                   nil)

;; QML items
(defvar *qml-edit*             "edit")
(defvar *qml-flick-edit*       "flick_edit")
(defvar *qml-command*          "command")
(defvar *qml-output*           "output")
(defvar *qml-clear*            "clear")
(defvar *qml-status*           "status")
(defvar *qml-clipboard-menu*   "clipboard_menu")
(defvar *qml-document-edit*    nil)
(defvar *qml-document-command* nil)

(defun read-file (file)
  (with-open-file (s (x:path file) :direction :input)
    (x:let-it (make-string (file-length s))
      (read-sequence x:it s))))

(defun ini-highlighters ()
  (setf *eql-keyword-format*  (qnew "QTextCharFormat")
        *lisp-keyword-format* (qnew "QTextCharFormat")
        *comment-format*      (qnew "QTextCharFormat")
        *lisp-match-rule*     (qnew "QRegExp(QString)" "[(']:*[^ )]+"))
  (|setForeground| *eql-keyword-format*  (qnew "QBrush(QColor)" "#0000C0"))
  (|setForeground| *lisp-keyword-format* (qnew "QBrush(QColor)" "#C00000"))
  (x:do-with *comment-format*
    (|setForeground| (qnew "QBrush(QColor)" "#80A080"))
    (|setFontItalic| t))
  (setf *highlighter-edit*    (qnew "QSyntaxHighlighter(QTextDocument*)" *qml-document-edit*)
        *highlighter-command* (qnew "QSyntaxHighlighter(QTextDocument*)" *qml-document-command*))
  (qoverride *highlighter-edit* "highlightBlock(QString)"
             (lambda (str) (highlight-block *highlighter-edit* str)))
  (qoverride *highlighter-command* "highlightBlock(QString)"
             (lambda (str) (highlight-block *highlighter-command* str))))

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
        (setf *cursor-indent* pos)
        (when (and (plusp pos)
                   (char= #\) (char line (1- pos))))
          (show-matching-paren text-cursor (subseq line 0 pos) :right))))))

;;; the following are workarounds because QML 'Keys' doesn't work on all devices

(let ((old 1))
  (defun edit-line-count-changed (new)
    (when (> new old)
      (return-pressed (1- old)))
    (setf old new))
  (defun reset-line-count ()
    (setf old 1)))

(let ((old 1))
  (defun command-line-count-changed (new)
    (if (> new old)
        (let ((line (remove #\Newline (qml-get *qml-command* "text"))))
          (qml-call *qml-command* "clear")
          (eval-expression line)
          (setf old 1))
    (setf old new))))

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

(defun return-pressed (line-number)
  (let ((spaces (indentation (|text| (|findBlockByLineNumber| *qml-document-edit* line-number)))))
    (unless (zerop spaces)
      (qml-call *qml-edit* "insert"
                (qml-get *qml-edit* "cursorPosition")
                (make-string spaces)))))

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
  (let ((max (|lineCount| (if (qml-get *qml-edit* "activeFocus")
                               *qml-document-edit*
                               *qml-document-command*))))
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

(defun active-edit ()
  (if (qml-get *qml-edit* "activeFocus")
      *qml-edit*
      *qml-command*))

(let ((ex-from -1))
  (defun show-matching-paren (text-cursor line type)
    (x:when-it (right-paren text-cursor line)
      (let* ((edit (active-edit))
             (set-y (string= edit *qml-edit*))
             (pos (|position| text-cursor))
             (from (- pos x:it 1))
             (color (qml-get edit "selectionColor")))
        (when (/= from ex-from)
          (setf ex-from from)
          (qsingle-shot 500 (lambda () (setf ex-from -1)))
          (let ((content-y (when set-y (qml-get *qml-flick-edit* "contentY"))))
            (qml-set edit "readOnly" t)
            (qml-set edit "selectionColor" "gray")
            (qml-call edit "select" from (1+ from))
            (qsleep 0.1)
            (qml-set edit "readOnly" nil)
            (qml-set edit "cursorPosition" pos)
            (qml-set edit "selectionColor" color)
            (when set-y
              (qml-set *qml-flick-edit* "contentY" content-y))))))))

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

(defun change-font (to &optional (steps 1))
  (let ((size (+ (qml-get *qml-edit* "font.pixelSize")
                 (* steps
                    (if (qml-get nil "isPhone") 1 2)
                    (if (eql :bigger to) 1 -1)))))
    (dolist (item (list *qml-edit* *qml-command* *qml-output* *qml-status*))
      (qml-set item "font.pixelSize" size))))

(defun clear ()
  (qml-call *qml-edit* "clear")
  (setf *file* nil)
  (reset-line-count))

;;; open file

(defun open-file ()
  (confirm-save-changes)
  (dialogs:get-file-name 'do-open-file))

(defun do-open-file ()
  (unless (x:empty-string dialogs:*file-name*)
    (if (probe-file dialogs:*file-name*)
        (progn
          (setf *file* dialogs:*file-name*)
          (if (x:starts-with "fas" (pathname-type *file*))
              (eval* (format nil "(load ~S)" *file*))
              (progn
                (qml-set *qml-edit* "text" (read-file *file*))
                (reset-line-count))))
        (qmsg (format nil "File does not exist:~%~%~S" dialogs:*file-name*)))))

;;; save-file

(defun save-to-file (file &optional (qml-item *qml-edit*) append)
  (with-open-file (s file :direction :output
                     :if-exists (if append :append :supersede)
                     :if-does-not-exist :create)
    (write-sequence (qml-get qml-item "text") s)
    (|clearUndoRedoStacks| *qml-document-edit*)))

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
  (let* ((edit (active-edit))
         (text (qml-get edit "text"))
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
          (qml-call edit "select" start end))))))

(defun select-all ()
  (setf *selection-start* nil)
  (qml-call (active-edit) "selectAll"))

(defun cut ()
  (copy)
  (qml-call (active-edit) "remove"
            *selection-start*
            (+ *selection-start* (length *copied-text*))))

(defun copy ()
  (let ((edit (active-edit)))
    (if *selection-start*
        (progn
          (setf *copied-text* *selected-text*)
          (let* ((snip (qml-call edit "getText" (max 0 (- *selection-start* 100)) *selection-start*))
                 (nl (position #\Newline snip :from-end t)))
            (setf *cursor-indent-copy* (if nl (- (length snip) (1+ nl)) 0))))
        (setf *copied-text*        (qml-get edit "text")
              *selection-start*    0
              *cursor-indent-copy* 0))))

(defun paste ()
  "Paste text adapting the indentation."
  (let ((edit (active-edit)))
    (when (and (string= *qml-command* edit)
               (find #\Newline *copied-text*))
      (return-from paste))
    (let* ((lines (x:split *copied-text* #\Newline))
           (diff (- *cursor-indent* *cursor-indent-copy*))
           (text (with-output-to-string (s)
                   (write-line (first lines) s)
                   (dolist (line (rest lines))
                     (when (plusp diff)
                       (write-string (make-string diff) s))
                     (write-line (subseq line (if (minusp diff) (- diff) 0)) s)))))
      (qml-call edit "insert"
                (qml-get edit "cursorPosition")
                (subseq text 0 (1- (length text)))))))

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

(let ((curr 0)
      (all 2))
  (defun set-text-document (name) ; called from QML
    ;; needed because QML-GET can't return QObject* pointers
    (setf (symbol-value (cond ((string= *qml-edit* name)
                               '*qml-document-edit*)
                              ((string= *qml-command* name)
                               '*qml-document-command*)))
          (|textDocument| qml:*caller*))
    (when (= all (incf curr))
      (ini-highlighters)
      (qconnect *qml-document-edit*    "blockCountChanged(int)" 'edit-line-count-changed)
      (qconnect *qml-document-command* "blockCountChanged(int)" 'command-line-count-changed)
      (qconnect *qml-document-edit*    "cursorPositionChanged(QTextCursor)" 'cursor-position-changed)
      (qconnect *qml-document-command* "cursorPositionChanged(QTextCursor)" 'cursor-position-changed)))
  (defun reset-documents ()
    (setf curr 0)))

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
  (flet ((clicked (name function &optional (timer t))
           (let ((item (find-quick-item name)))
             (qconnect item "clicked()" function)
             (when timer
               (qconnect item "clicked()" 'start-menu-timer)))))
    (clicked "undo"            (lambda () (qml-call *qml-edit* "undo")))
    (clicked "redo"            (lambda () (qml-call *qml-edit* "redo")))
    (clicked "font_bigger"     (lambda () (change-font :bigger)))
    (clicked "font_smaller"    (lambda () (change-font :smaller)))
    (clicked "clear"           'clear)
    (clicked "open_file"       'open-file)
    (clicked "save_file"       'save-file)
    (clicked "eval"            'eval-expression)
    (clicked "history_back"    (lambda () (history-move :back)) nil)
    (clicked "history_forward" (lambda () (history-move :forward)) nil)))

;;; auto hide menues

(defvar *menu-timer* nil)

(defvar *qml-hide-buttons-top*   "hide_buttons_top")
(defvar *qml-hide-buttons-right* "hide_buttons_right")

(defun start-menu-timer ()
  (unless *menu-timer*
    (setf *menu-timer* (qnew "QTimer" "singleShot" t))
    (qconnect *menu-timer* "timeout()"
              (lambda ()
                (qml-call *qml-hide-buttons-top* "start")
                (qml-call *qml-hide-buttons-right* "start"))))
  (|start| *menu-timer* 3000))

;;; cursor movement (see arrow buttons in QML)

(defvar *focus-editor* *qml-command*)

(defun connect-arrows ()
  (flet ((pressed (name function)
           (qconnect (find-quick-item name) "pressed()" function)))
    (pressed "up"       (lambda () (arrow-pressed "up")))
    (pressed "down"     (lambda () (arrow-pressed "down")))
    (pressed "left"     (lambda () (arrow-pressed "left")))
    (pressed "right"    (lambda () (arrow-pressed "right")))
    (pressed "keyboard" (lambda () (ensure-focus :show)))))

(defun set-focus-editor (qml-name) ; called from QML
  (setf *focus-editor* qml-name))

(defun ensure-focus (&optional show) ; called from QML (and Lisp)
  (qml-call *focus-editor* "forceActiveFocus")
  (funcall (if show '|show| '|hide|) (|inputMethod.QGuiApplication|)))

(defun arrow-pressed (name)
  (let* ((dir (intern (string-upcase name) :keyword))
         (pos (qml-get *focus-editor* "cursorPosition"))
         (new-pos (cond ((find dir '(:left :right))
                         (+ pos (if (eql :right dir) 1 -1)))
                        (t
                         (let ((rect (qml-get *focus-editor* "cursorRectangle")))
                           (qml-call *focus-editor* "positionAt"
                                     (truncate (first rect))
                                     (truncate (+ (second rect)
                                                  (if (eql :down dir)
                                                      (1+ (fourth rect))
                                                      -1)))))))))
    (qml-set *focus-editor* "cursorPosition"
             (max 0 (min new-pos (qml-get *focus-editor* "length"))))))

;;; start

(defun start ()
  (qlater 'eql-user::ini) ; for Swank, Quicklisp
  (qml:ini-quick-view "qml/repl.qml")
  (when (qml-get nil "isPhone")
    (change-font :smaller 3))
  (connect-buttons)
  (connect-menu-buttons)
  (connect-arrows)
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
            :debug-dialog 'dialogs:debug-dialog))

(defun reload-qml (&optional (url "http://localhost:8080/"))
  ;; please see README-1.md
  "Reload QML file from an url, directly on the device."
  (setf eql::*reloading-qml* t)
  (reset-documents)
  (let ((src (|toString| (|source| qml:*quick-view*))))
    (if (x:starts-with "qrc:/" src)
        (|setSource| qml:*quick-view* (qnew "QUrl(QString)"
                                            (x:string-substitute url "qrc:/" src)))
        (qml:reload))
    (|toString| (|source| qml:*quick-view*))))

(defun qml-reloaded ()
  (when (qml-get nil "isPhone")
    (change-font :smaller 3))
  (setf *focus-editor* *qml-command*)
  (connect-buttons)
  (connect-menu-buttons)
  (connect-arrows)
  (setf eql::*reloading-qml* nil))
