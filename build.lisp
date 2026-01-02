;;;; build.lisp

(load "displayer.asd")

(ql:quickload :lp-displayer)

(defun entry ()
  "Application entrypoint forcing it to run inside package namespace"
  (setf *package* (find-package :lp-displayer))
  (lp-displayer:main))

(sb-ext:save-lisp-and-die "lp-displayer"
                   :executable t
                   :compression 9
                   :toplevel #'entry)
