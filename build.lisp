;;;; build.lisp

(load "realtor.asd")

(ql:quickload :screen-realtor)

(defun entry ()
  "Application entrypoint forcing it to run inside package namespace"
  (setf *package* (find-package :screen-realtor))
  (screen-realtor:main))

(sb-ext:save-lisp-and-die "screen-realtor"
                   :executable t
                   :compression 9
                   :toplevel #'entry)
