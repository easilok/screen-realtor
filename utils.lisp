(in-package #:screen-realtor)

(defun ensure-symbol (v)
  "Returns the provided value in it's symbol representation if possible"
  (cond
    ((symbolp v) v)
    ((stringp v)(intern (string-upcase v)))
    (t (error "Could not convert the type '~a' to symbol" (type-of v)))))
