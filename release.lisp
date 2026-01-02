;;;; release.lisp

;; Installs quicklisp for building executable
(load "quicklisp.lisp")
(quicklisp-quickstart:install)

(push (truename ".") asdf:*central-registry*)

;; Loads build executable script
(load "build.lisp")
