;;; write all external EQL symbols in a file.
;;; (includes both Qt enums and Qt wrapper functions)

(with-open-file (out "intern-symbols.lisp" :direction :output :if-exists :supersede)
  (flet ((run (&optional enums)
           (let (symbols)
             (do-external-symbols (sym (find-package :eql))
               (when (funcall (if enums 'identity 'not) (find #\. (symbol-name sym)))
                 (unless (eql '! sym)
                   (push sym symbols))))
             (dolist (sym (sort symbols 'string< :key 'symbol-name))
               (format out "(export (intern \"~A\" :eql) :eql)~%" sym out)))))
    (run)
    (run :enums)))

(qq)

