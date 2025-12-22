;;;; conditions.lisp

(in-package #:lp-displayer)

(define-condition display-error (error) ())

(define-condition ambiguous-output-match (display-error)
  ((selector :initarg :selector)
   (matches  :initarg :matches))
  (:report
   (lambda (c s)
     (format s "Selector ~S matched multiple outputs: ~{~A~^, ~}"
             (slot-value c 'selector)
             (mapcar #'output-name (slot-value c 'matches))))))

(define-condition no-output-match (display-error)
  ((selector :initarg :selector))
  (:report
   (lambda (c s)
     (format s "Selector ~S matched no outputs"
             (slot-value c 'selector)))))

(define-condition unsupported-mode (display-error)
  ((output :initarg :output)
   (mode   :initarg :mode)))

(define-condition invalid-layout (display-error)
  ((reason :initarg :reason)))
