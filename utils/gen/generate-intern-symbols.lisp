;;; write all external EQL symbols in a file.
;;; (includes both Qt enums and Qt wrapper functions)

(with-open-file (out "intern-symbols.lisp" :direction :output :if-exists :supersede)
  (flet ((run (&optional enums)
           (let (symbols)
             (do-external-symbols (sym (find-package :eql))
               (unless (eql '! sym)
                 (let* ((name (symbol-name sym))
                        (is-enum (and (find #\. name)
                                      (upper-case-p (char name 0))))
                        (add-enum (and enums is-enum)))
                   (when (or add-enum
                             (and (not enums) (not is-enum)))
                     (push sym symbols)))))
             (dolist (sym (sort symbols 'string< :key 'symbol-name))
               (format out "(export (intern \"~A\"))~%" sym out)
               (when (and (boundp sym)
                          (find #\. (symbol-name sym)))
                 ;; set enum value (to be able to cross-compile reader macros, e.g. in CASE)
                 (format out "(setf |~A| ~D)~%"
                         (symbol-name sym)
                         (symbol-value sym)
                         out))))))
    (run)
    (run :enums)))

(qq)

