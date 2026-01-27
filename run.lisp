;;;; run.lisp

(load "realtor.asd")

(ql:quickload :screen-realtor)

(in-package #:screen-realtor)

(screen-realtor:main)
