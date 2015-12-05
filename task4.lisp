(in-package 'COMMON-LISP-USER)

(defconstant +alphabet+ "0123456789abcdefghijklmnopqrstuvw")

(defun rebase (num base &optional (items ()))
  (multiple-value-bind (foo bar) (truncate num base)
    (let ((items-next (cons (char +alphabet+ bar) items)))
      (if (= foo 0)
          (format nil "~{~A~^~}" items-next)
          (rebase foo base items-next)))))

(defun bin (num)
  (rebase num 2))

(defun hex (num)
  (rebase num 16))
