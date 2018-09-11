;;; This is a simplified / adapted version of EQL example 9

(in-package :editor)

(qrequire :quick)

(dolist (module (list :multimedia :network :sql :svg))
  (qrequire module :quiet)) ; load if available

;;; colors (after changes, call APPLY-COLORS)

(defvar *text-color*              "black")
(defvar *background-color*        "white")
(defvar *parenthesis-color*       "lightslategray")
(defvar *string-color*            "saddlebrown")
(defvar *comment-color*           "lightslategray")
(defvar *lisp-keyword-color*      "#c05050")
(defvar *eql-keyword-color*       "#5050c0")
(defvar *keyword-color*           "#409090")

(defvar *output-text-color*       "black")
(defvar *output-background-color* "lavender")
(defvar *output-string-color*     "saddlebrown")
(defvar *output-value-color*      "#2020ff")
(defvar *output-trace-color*      "darkmagenta")
(defvar *output-error-color*      "red")

(defun apply-colors ()
  (x:do-with *lisp-keyword-format*
    (|setForeground| (qnew "QBrush(QColor)" *lisp-keyword-color*))
    (|setFontWeight| |QFont.Bold|))
  (x:do-with *eql-keyword-format*
    (|setForeground| (qnew "QBrush(QColor)" *eql-keyword-color*))
    (|setFontWeight| |QFont.Bold|))
  (x:do-with *keyword-format*
    (|setForeground| (qnew "QBrush(QColor)" *keyword-color*))
    (|setFontWeight| |QFont.Bold|))
  (x:do-with *comment-format*
    (|setForeground| (qnew "QBrush(QColor)" *comment-color*))
    (|setFontItalic| t))
  (dolist (item/color (list (cons *qml-edit*         *text-color*)
                            (cons *qml-rect-edit*    *background-color*)
                            (cons *qml-command*      *text-color*)
                            (cons *qml-rect-command* *background-color*)
                            (cons *qml-output*       *output-text-color*)
                            (cons *qml-rect-output*  *output-background-color*)))
    (qml-set (car item/color) "color"
             (cdr item/color)))
  (unless (zerop (qml-get *qml-edit* "length"))
    (dolist (fun '("selectAll" "cut" "paste")) ; apply to editor
      (qml-call *qml-edit* fun))))

;;;

(defvar *package-char-dummy*     #\$)
(defvar *separator*              "#||#")
(defvar *lisp-match-rule*        nil)
(defvar *keyword-match-rule*     nil)
(defvar *lisp-keyword-format*    nil)
(defvar *eql-keyword-format*     nil)
(defvar *keyword-format*         nil)
(defvar *comment-format*         nil)
(defvar *current-depth*          0)
(defvar *current-keyword-indent* 0)
(defvar *cursor-indent*          0)
(defvar *highlighter-edit*       nil)
(defvar *highlighter-command*    nil)
(defvar *file*                   nil)

;; QML items
(defvar *qml-main*             "main")
(defvar *qml-edit*             "edit")
(defvar *qml-rect-edit*        "rect_edit")
(defvar *qml-flick-edit*       "flick_edit")
(defvar *qml-command*          "command")
(defvar *qml-rect-command*     "rect_command")
(defvar *qml-output*           "output")
(defvar *qml-rect-output*      "rect_output")
(defvar *qml-flick-output*     "flick_output")
(defvar *qml-clear*            "clear")
(defvar *qml-clipboard-menu*   "clipboard_menu")
(defvar *qml-document-edit*    nil)
(defvar *qml-document-command* nil)

(defmacro enum (&rest names/values)
  `(progn
     ,@(mapcar (lambda (n/v) `(defconstant ,(car n/v) ,(cdr n/v)))
               names/values)))

(defun read-file (file)
  (with-open-file (s (x:path file))
    (x:let-it (make-string (file-length s))
      (read-sequence x:it s))))

(defun ini-highlighters ()
  (setf *lisp-keyword-format* (qnew "QTextCharFormat")
        *eql-keyword-format*  (qnew "QTextCharFormat")
        *keyword-format*      (qnew "QTextCharFormat")
        *comment-format*      (qnew "QTextCharFormat")
        *lisp-match-rule*     (qnew "QRegExp(QString)" "[(']:*[^ )]+")
        *keyword-match-rule*  (qnew "QRegExp(QString)" "[(': ]?[*:&][a-z1-9\\-*]*"))
  (setf *highlighter-edit*    (qnew "QSyntaxHighlighter(QTextDocument*)" *qml-document-edit*)
        *highlighter-command* (qnew "QSyntaxHighlighter(QTextDocument*)" *qml-document-command*))
  (qoverride *highlighter-edit* "highlightBlock(QString)"
             (lambda (str) (highlight-block *highlighter-edit* str)))
  (qoverride *highlighter-command* "highlightBlock(QString)"
             (lambda (str) (highlight-block *highlighter-command* str))))

(defun read* (string &optional (start 0))
  (flet ((no-package-colons (str)
           (let ((str* (copy-seq str))
                 (ex #\Space))
             (dotimes (i (length str*))
               (let ((ch (char str* i)))
                 (when (and (char= #\: ch)
                            (char/= #\# ex))
                   (setf (char str* i) *package-char-dummy*))
                 (setf ex ch)))
             str*)))
    (let ((*package* #.(find-package :eql)))
      (multiple-value-bind (exp x)
          (ignore-errors (read-from-string (no-package-colons string)
                                           nil nil :start start :preserve-whitespace t))
        (values exp x)))))

(defun end-position (string)
  (multiple-value-bind (x end)
      (read* string)
    (when (numberp end)
      end)))

;;; syntax highlighting

(enum
 (+no-value+   . -1)
 (+in-comment+ . 1)
 (+in-string+  . 2))

(defun highlight-block (highlighter text)
  ;; CL, EQL5 functions and macros
  (let ((i (|indexIn| *lisp-match-rule* text)))
    (x:while (>= i 0)
      (let* ((len (|matchedLength| *lisp-match-rule*))
             (kw* (string-left-trim "(" (subseq text (1+ i) (+ i len))))
             (kw (x:if-it (position #\: kw* :from-end t)
                          (subseq kw* (1+ x:it))
                          kw*)))
        (flet ((set-format (frm)
                 (|setFormat| highlighter (1+ i) (1- len) frm)))
          (cond ((gethash kw *lisp-keywords*)
                 (set-format *lisp-keyword-format*))
                ((find kw *eql-keywords-list* :test 'string=)
                 (set-format *eql-keyword-format*))))
        (setf i (|indexIn| *lisp-match-rule* text (+ i len))))))
  ;; CL keywords etc.
  (let ((i (|indexIn| *keyword-match-rule* text))
        (extra "(' "))
    (x:while (>= i 0)
      (let* ((len (|matchedLength| *keyword-match-rule*))
             (kw (subseq text i (+ i len))))
        (when (gethash (string-left-trim extra kw) *keywords*)
          (let ((skip (find (char kw 0) extra)))
            (unless (and (not skip) (plusp i))
              (|setFormat| highlighter
                           (if skip (1+ i) i)
                           (if skip (1- len) len)
                           *keyword-format*))))
        (setf i (|indexIn| *keyword-match-rule* text (+ i len))))))
  ;; comments, strings, parenthesis
  (flet ((set-comment-format (pos len)
           (|setFormat| highlighter pos len *comment-format*))
         (set-color (pos len color)
           (|setFormat| highlighter pos len color))
         (set-state (state)
           (|setCurrentBlockState| highlighter state)))
    (let* ((ex #\Space)
           (state (|currentBlockState| highlighter))
           (prev-state (|previousBlockState| highlighter))
           (in-string (= +in-string+ prev-state))    ; multi line strings
           (in-comment (= +in-comment+ prev-state))) ; multi line comments
      (set-state (if (x:empty-string text) prev-state +no-value+))
      (dotimes (i (length text))
        (let ((ch (char text i)))
          (unless (char= #\\ ex)
            (cond (;; multi line comment
                   (or (and (char= #\# ex)
                            (char= #\| ch))
                       (and (zerop i)
                            in-comment))
                   (let ((len (x:if-it (search "|#" (subseq text i))
                                       (+ x:it (if in-comment 2 3))
                                       (progn
                                         (set-state +in-comment+)
                                         (length text)))))
                     (set-comment-format (max (1- i) 0) len)
                     (incf i (1- len)))
                   (force-repaint)
                   (setf ch #\Space))
                  ;; single/multi line string
                  ((or (char= #\" ch)
                       (and (zerop i)
                            in-string))
                   (let ((len (x:if-it (end-position (if in-string (x:cc "\"" text) (subseq text i)))
                                       (if in-string (1- x:it) x:it)
                                       (progn
                                         (set-state +in-string+)
                                         (length text)))))
                     (set-color i len *string-color*)
                     (incf i (1- len)))
                   (setf ch #\Space))
                  ;; parens
                  ((find ch "()")
                   (set-color i 1 *parenthesis-color*))
                  ;; single line comment
                  ((char= #\; ch)
                   (set-comment-format i (- (length text) i))
                   (return))))
          (setf ex ch)))
      (when (/= state (|currentBlockState| highlighter))
        (force-repaint)))))

(let (timer)
  (defun force-repaint ()
    (when (string= *qml-edit* (active-edit))
      ;; N.B. workaround for bug (missing repaint after multi line highlight changes)
      ;; (happening in e.g. Qt 5.10)
      (unless timer
        (setf timer (qnew "QTimer"
                          "singleShot" t
                          "interval" 100))
        (qconnect timer "timeout()"
                  ;; called max once every 100 ms
                  (lambda ()
                    (let ((pos (qml-get *qml-edit* "cursorPosition"))
                          (len (qml-get *qml-edit* "length")))
                      (when (plusp len)
                        (qml-set *qml-main* "skipEnsureVisible" t)
                        (qml-call *qml-edit* "select" (1- len) len)
                        (qml-set *qml-edit* "cursorPosition" pos)
                        (qml-set *qml-main* "skipEnsureVisible" nil))))))
      (|start| timer))))

;;; auto completion: start when 2 spaces have been inserted in less than 500 ms

(let ((pos 0))
  (defun edit-contents-changed ()
    (let ((pos* (qml-get *qml-edit* "cursorPosition")))
      (when (= pos* (1+ pos))
        (qlater (lambda () (contents-changed *qml-document-edit* (1- pos)))))
      (setf pos pos*))))

(let ((pos 0))
  (defun command-contents-changed ()
    (let ((pos* (qml-get *qml-command* "cursorPosition")))
      (when (= pos* (1+ pos))
        (qlater (lambda () (contents-changed *qml-document-command* (1- pos)))))
      (setf pos pos*))))

(let ((space-count 0))
  (defun contents-changed (document position)
    (when (and (plusp position)
               (char= #\Space (|characterAt| document position)))
      (qsingle-shot 500 (lambda () (setf space-count 0)))
      (when (= 2 (incf space-count))
        (let ((ch (|characterAt| document (- position 2))))
          (when (alphanumericp ch)
            (let ((start (- position 3))
                  (text (list ch)))
              (x:while (and (not (minusp start))
                            (or (alphanumericp (setf ch (|characterAt| document start)))
                                (find ch "-:&*")))
                (decf start)
                (push ch text))
              (search-completion (coerce text 'string)))))))))

(defun search-completion (short)
  (let ((p (position #\: short)))
    (when (and p (plusp p))
      (setf short (subseq short (1+ p)))))
  (let* ((edit (active-edit))
         (pos (qml-get edit "cursorPosition")))
    (qml-call edit "remove" (- pos 2) pos) ; remove the 2 spaces
    (x:when-it (complete-symbol short)
      (setf pos (qml-get edit "cursorPosition"))
      (let ((pos-2 (- pos (length short))))
        (qml-call edit "remove" pos-2 pos)
        (qml-call edit "insert" pos-2 x:it)))))

(defun complete-symbol (short)
  "Works only for generic CL symbols / CL keywords / global CL variables / EQL5 symbols and respective abbreviations."
  (if (find #\- short)
      ;; complete an abbreviation; example: "m-v-b" => "multiple-value-bind"
      ;; (QRegExp is more convenient here than CL-PPCRE)
      (progn
        (setf short (x:string-substitute "\\*" "*" short))
        (qlet ((regex "QRegExp(QString)"
                      (x:cc (x:string-substitute "[a-z1-9:&*]*-" "-" short)
                            "[a-z1-9\\-*]*")))
          (dolist (names (list *lisp-keywords-list*
                               *keywords-list*
                               *eql-keywords-list*))
            (dolist (name names)
              (when (|exactMatch| regex name)
                (return-from complete-symbol name))))))
      ;; complete as far as unambiguous; return full name if unique
      (let (matches)
        (dolist (names (list *lisp-keywords-list*
                             *keywords-list*
                             *eql-keywords-list*))
          (dolist (name names)
            (when (x:starts-with short name)
              (push name matches))))
        (when matches
          (if (rest matches)
              (let ((i1 (1+ (length short)))
                    (i2 (apply 'min (mapcar 'length matches))))
                (do ((i i1 (1+ i)))
                    ((> i i2) (subseq (first matches) 0 (1- i)))
                  (let ((start (subseq (first matches) 0 i)))
                    (unless (every (lambda (str) (x:starts-with start str))
                                   matches)
                      (return-from complete-symbol (subseq start 0 (1- (length start))))))))
              (first matches))))))

;;; the following are workarounds because QML 'Keys' doesn't work on all devices

(defun current-line ()
  (let* ((rect (qml-get *qml-edit* "cursorRectangle"))
         (y (second rect))
         (h (fourth rect)))
    (when (plusp h)
      (1- (truncate (/ y h))))))

(let ((old 1))
  (defun edit-line-count-changed (new)
    (when (> new old)
      (x:when-it (current-line)
        (return-pressed x:it)))
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
   do-all-symbols do-external-symbols do-symbols flet labels lambda let let*
   loop multiple-value-bind prog progn prog1 prog2 qlet typecase unless when
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
           (in-local       (find (read* (reverse (subseq code 0 pos-local)))
                                 '(flet labels macrolet)))
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

(defvar *left-paren-indent*  nil)
(defvar *closing-all-parens* nil)

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
                (x:empty-string (string-trim " " text))))
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

(defun cursor-position-changed (text-cursor)
  (let* ((text-block (|block| text-cursor))
         (line (|text| text-block))
         (pos (|positionInBlock| text-cursor)))
    (setf *cursor-indent* pos)
    (when (and (plusp pos)
               (char= #\) (char line (1- pos))))
      (show-matching-paren text-cursor (subseq line 0 pos) :right))))

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
            (qml-set edit "selectionColor" "gray")
            (qml-call edit "select" from (1+ from))
            (setf *left-paren-indent*
                  (> (first (qml-call edit "positionToRectangle" from))
                     4)) ; pixel indent of QML "command"
            (unless *closing-all-parens*
              (qprocess-events)
              (sleep 0.15))
            (qml-set edit "cursorPosition" pos)
            (qml-set edit "selectionColor" color)
            (when set-y
              (qml-set *qml-flick-edit* "contentY" content-y)))
          (when *closing-all-parens*
            (qlater 'do-close-all-parens)))))))

(let (n)
  (defun close-all-parens ()
    (setf n                    25 ; limit (to be safe on tilts)
          *closing-all-parens* t)
    (insert-closing-paren))
  (defun do-close-all-parens ()
    (if (and *left-paren-indent*
             (plusp (decf n)))
        (insert-closing-paren)
        (setf *closing-all-parens* nil)))
  (defun insert-closing-paren ()
    (qsingle-shot 50 (lambda () (insert ")")))))

;;; eval

(defun eval* (text)
  (if (find #\Newline text)
      (eval:feed-top-level text)
      (let ((text* (string-trim " " text)))
        (flet ((cmd (str)
                 (string-equal str text*)))
          (if (cmd ":k")
              (if eval:*eval-thread*
                  (progn
                    (mp:process-kill eval:*eval-thread*)
                    (setf eval:*eval-thread* nil)
                    (eval::set-eval-state nil)
                    (eval::clear-buffers)
                    (print-eval-output :error ":KILLED"))
                  (print-eval-output :values "kill: eval thread not running"))
              (let ((cmd (cond ((cmd ":h")
                                "(eql:help)")
                               ((cmd ":s")
                                "(eql:start-swank)")
                               ((cmd ":q")
                                "(eql:quicklisp)")
                               ((cmd ":a")
                                "(require :asdf)")
                               ((cmd ":f")
                                "(dialogs:get-file-name)")
                               ((cmd ":c")
                                "(progn (qml:qml-call editor::*qml-output* \"clear\") (values))")
                               ((cmd ":r")
                                "(editor:reload-qml)") ; see README in example 'my'
                               ((cmd ":u")
                                "(eql:install-update)")
                               ((cmd "*")
                                (format nil "(progn~%  (editor::set-clipboard-text (prin1-to-string *))~%  *)")))))
                (eval:feed-top-level (or cmd text))))))))

(defun print-eval-output (type text)
  (let ((text* (qescape text))
        (bold (not (eql :expression type))))
    ;; "insert" is cleaner with formatting than "append"
    (qml-call *qml-output* "insert"
              (qml-get *qml-output* "length")
              (format nil "<pre><font face='Hack' color='~A'>~A~%~A~A</font></pre>"
                      (case type
                        (:output *output-string-color*)
                        (:values *output-value-color*)
                        (:trace  *output-trace-color*)
                        (:error  *output-error-color*)
                        (t       *output-text-color*))
                      (if bold "<b>" "")
                      (if (eql :values type)
                          (x:string-substitute "<br>" *separator* text*)
                          (x:string-substitute "<br>" (string #\Newline) text*))
                      (if bold "</b>" ""))))
  (qml-set *qml-output* "cursorPosition"
           (qml-get *qml-output* "length"))
  (qml-set *qml-flick-output* "contentX" 0))

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
          (with-open-file (s *history-file*)
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
    (when (and *history-index*
               (plusp (length *history*)))
      (setf *history-index* (if (eql :back dir)
                                (max (1- *history-index*) 0)
                                (min (1+ *history-index*) (1- (length *history*)))))
      (let ((text (aref *history* *history-index*)))
        (qml-set *qml-command* "text" text)
        (qml-set *qml-command* "cursorPosition"
                 (- (length text) (if (x:ends-with ")" text) 1 0)))))))

;;; etc.

(defun change-font (to &optional (steps 1))
  (let ((size (+ (qml-get *qml-edit* "font.pixelSize")
                 (* steps
                    (if (qml-get nil "isPhone") 1 2)
                    (if (eql :bigger to) 1 -1)))))
    (dolist (item (list *qml-edit* *qml-command* *qml-output*))
      (qml-set item "font.pixelSize" size))))

(defun clear ()
  (qml-call *qml-edit* "clear")
  (setf *file* nil)
  (setf *current-keyword-indent* 0
        *cursor-indent*          0)
  (reset-line-count))

(defun insert (text)
  (let ((edit (active-edit)))
    ;; QLATER: prevent blocking on fast, repeated calls
    (qlater (lambda ()
              (qml-call edit "insert"
                        (qml-get edit "cursorPosition")
                        text)))))

;;; open file

(defun open-file ()
  (save-changes :confirm)
  (dialogs:get-file-name 'do-open-file))

(defun do-open-file ()
  (unless (x:empty-string dialogs:*file-name*)
    (if (probe-file dialogs:*file-name*)
        (if (x:starts-with "fas" (pathname-type dialogs:*file-name*))
            (qsingle-shot 150 (lambda () (eval* (format nil "(load ~S)" dialogs:*file-name*)))) ; wait for dialog to be hidden
            (progn
              (setf *file* dialogs:*file-name*)
              (qml-set *qml-edit* "text" (read-file *file*))
              (reset-line-count)))
        (qmsg (format nil "File does not exist:~%~%~S" dialogs:*file-name*)))))

;;; save-file

(defun save-to-file (file)
  (ensure-directories-exist file)
  (with-open-file (s file :direction :output
                     :if-exists :supersede)
    (write-sequence (qml-get *qml-edit* "text") s)
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
      (let ((type (pathname-type dialogs:*file-name*)))
        (when (and (string/= ".eclrc" (pathname-name dialogs:*file-name*))
                   (or (not type)
                       (not (find type '("lisp" "lsp" "asd" "exp" "sexp" "fas" "fasb" "fasc")
                                  :test 'string-equal))))
          (setf dialogs:*file-name* (x:cc dialogs:*file-name* ".lisp"))))
      (when (probe-file dialogs:*file-name*)
        (unless (confirm-save-dialog "Overwrite?"
                                     (format nil "File already exists; overwrite?<br><br>~S<br>" dialogs:*file-name*))
          (setf ok nil)))
      (when ok
        (setf *file* dialogs:*file-name*)
        (save-to-file *file*)))))

(defun save-changes (&optional confirm)
  (when (and *file*
             (qml-get *qml-edit* "canUndo")
             (if confirm
                 (confirm-save-dialog "Save changes?"
                                      (format nil "Save changes to current file?<br><br>~S<br>" *file*))
                 t))
    (save-to-file *file*)))

;;; select all, cut, copy, paste

(defvar *selected-text*      "")
(defvar *selection-start*    0)
(defvar *cursor-indent-copy* 0)

(defun clipboard-text ()
  (|text| (|clipboard.QGuiApplication|)))

(defun set-clipboard-text (text)
  (|setText| (|clipboard.QGuiApplication|) text))

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
      (when (and (plusp start)
                 (char= #\` (char text (1- start))))
        (decf start))
      (x:when-it (end-position (subseq text start))
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
            (+ *selection-start* (length (clipboard-text)))))

(defun copy ()
  (let ((edit (active-edit)))
    (if *selection-start*
        (progn
          (set-clipboard-text *selected-text*)
          (let* ((snip (qml-call edit "getText" (max 0 (- *selection-start* 100)) *selection-start*))
                 (nl (position #\Newline snip :from-end t)))
            (setf *cursor-indent-copy* (if nl (- (length snip) (1+ nl)) 0))))
        (progn
          (set-clipboard-text (qml-get edit "text"))
          (setf *selection-start*    0
                *cursor-indent-copy* 0)))))

(defun paste ()
  "Paste text adapting the indentation."
  (let ((edit (active-edit))
        (clip-text (clipboard-text)))
    (when (and (string= *qml-command* edit)
               (find #\Newline clip-text))
      (return-from paste))
    (unless (x:empty-string clip-text)
      (let* ((lines (x:split clip-text #\Newline))
             (diff (- *cursor-indent* *cursor-indent-copy*))
             (text (with-output-to-string (s)
                     (write-line (first lines) s)
                     (dolist (line (rest lines))
                       (when (plusp diff)
                         (write-string (make-string diff) s))
                       (write-line (subseq line (if (minusp diff) (- diff) 0)) s)))))
        (qml-call edit "remove"
                  (qml-get edit "selectionStart")
                  (qml-get edit "selectionEnd"))
        (qml-call edit "insert"
                  (qml-get edit "cursorPosition")
                  (subseq text 0 (1- (length text))))))))

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
      (qconnect *qml-document-command* "cursorPositionChanged(QTextCursor)" 'cursor-position-changed)
      (qconnect *qml-document-edit*    "contentsChanged()" 'edit-contents-changed)
      (qconnect *qml-document-command* "contentsChanged()" 'command-contents-changed)))
  (defun reset-documents ()
    (setf curr 0)))

(defun delayed-ini () ; called from QML
  (qlater 'apply-colors)
  ;; needed because resizing sometimes gets messed up on startup
  ;; (caused by virtual keyboard)
  (qsingle-shot 1000 (lambda ()
                       (qml-call *qml-command* "forceActiveFocus")
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
(defvar *qml-buttons-top*        "buttons_top")

(defun start-menu-timer ()
  (unless *menu-timer*
    (setf *menu-timer* (qnew "QTimer" "singleShot" t))
    (qconnect *menu-timer* "timeout()"
              (lambda ()
                (when (zerop (qml-get *qml-buttons-top* "y"))
                  (qml-call *qml-hide-buttons-top* "start")
                  (qml-call *qml-hide-buttons-right* "start")))))
  (|start| *menu-timer* 3000))

;;; cursor movement (see arrow buttons in QML)

(defvar *focus-editor* *qml-command*)

(defun connect-arrows ()
  (labels ((connect (name qsignal function)
             (qconnect (find-quick-item name) qsignal function))
           (pressed (name function)
             (connect name "pressed()" function))
           (press-and-hold (name function)
             (connect name "pressAndHold()" function)))
    (pressed "up"       (lambda () (arrow-pressed :up)))
    (pressed "down"     (lambda () (arrow-pressed :down)))
    (pressed "left"     (lambda () (arrow-pressed :left)))
    (pressed "right"    (lambda () (arrow-pressed :right)))
    (pressed "keyboard" (lambda () (ensure-focus :show)))
    (press-and-hold "up"    (lambda () (arrow-helt :up)))
    (press-and-hold "down"  (lambda () (arrow-helt :down)))
    (press-and-hold "left"  (lambda () (arrow-helt :left)))
    (press-and-hold "right" (lambda () (arrow-helt :right)))))

(defun set-focus-editor (qml-name) ; called from QML
  (setf *focus-editor* qml-name))

(defun ensure-focus (&optional show) ; called from QML (and Lisp)
  (qml-call *focus-editor* "forceActiveFocus")
  (funcall (if show '|show| '|hide|) (|inputMethod.QGuiApplication|)))

(defun arrow-pressed (direction)
  (let ((new-pos (if (find direction '(:left :right))
                     (+ (qml-get *focus-editor* "cursorPosition")
                        (if (eql :right direction) 1 -1))
                     (let ((rect (qml-get *focus-editor* "cursorRectangle")))
                       (qml-call *focus-editor* "positionAt"
                                 (truncate (first rect))
                                 (truncate (+ (second rect)
                                              (if (eql :down direction)
                                                  (1+ (fourth rect))
                                                  -1))))))))
    (qml-set *focus-editor* "cursorPosition"
             (max 0 (min new-pos (qml-get *focus-editor* "length"))))))

(defun arrow-helt (direction)
  (let ((rect (qml-get *focus-editor* "cursorRectangle")))
    (qml-set *focus-editor* "cursorPosition"
             (qml-call *focus-editor* "positionAt"
                       (case direction
                         ((:left :up)
                          0)
                         (t
                          (qml-get *focus-editor* "paintedWidth")))
                       (case direction
                         ((:left :right)
                          (1+ (second rect)))
                         (:up
                          0)
                         (:down
                          (qml-get *focus-editor* "paintedHeight")))))))

;;; start

(defun start ()
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
  (eval:ini :output       'print-eval-output
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

;;; quit app

(defun back-pressed () ; called from QML
  (or (dialogs:pop-dialog)
      (progn
        (save-changes)
        (qquit))))
