(in-package :COMMON-LISP-USER)

(defconstant +alphabet+ "0123456789abcdefghijklmnopqrstuvw")

(defun rebase (num base &optional (items ()))
  (multiple-value-bind (div-part mod-part)
      (truncate num base)
    (let ((items-next (cons (char +alphabet+ mod-part) items)))
      (if (= div-part 0)
          (format nil "~{~A~^~}" items-next)
          (rebase div-part base items-next)))))

(defun bin (num)
  (rebase num 2))

(defun oct (num)
  (rebase num 8))

(defun hex (num)
  (rebase num 16))

(defun maya (num)
  (rebase num 20))

(defun run-tests ()
  (assert (string= (bin 1) "1"))
  (assert (string= (bin 2) "10"))
  (assert (string= (bin 10) "1010"))
  (assert (string= (bin 11) "1011"))
  (assert (string= (bin 255) "11111111"))
  (assert (string= (oct 8) "10"))
  (assert (string= (oct 1234) "2322"))
  (assert (string= (hex 1) "1"))
  (assert (string= (hex 10) "a"))
  (assert (string= (hex 15) "f"))
  (assert (string= (hex 255) "ff"))
  (assert (string= (hex (* 256 256)) "10000"))
  (assert (string= (maya 20) "10"))
  (assert (string= (maya 19) "j"))
  (format *standard-output* "Tests OK"))
