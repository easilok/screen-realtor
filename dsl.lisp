;;;; dsl.lisp

(in-package #:lp-displayer)

(defmacro output (selector &key mode position primary)
  "Parses an output configuration from the DSL configuration file into it's own object instance"
  `(make-instance 'output-configuration
     :selector ,selector
     :mode ',mode
     :position ',position
     :primary-p ,(if primary primary nil)))

(defmacro define-layout (name outputs)
  "Parses a layout configuration from the DSL configuration file into it's own object instance
   Builds a local hash table of layouts configuration as *layouts*"
  `(let ((layout (make-instance 'layout
                                :name ',name
                                :outputs (list ,@outputs))))
     (setf (gethash ',name *layouts*) layout)
     layout))

(defmacro define-rule (name &key predicate layout priority)
  "Parses a rule configuration from the DSL configuration file into it's own object instance
   Builds a local list of rules configuration to iterate over as *rules*"
  `(let* ((default-priority (+ 100 (length *rules*)))
          (rule (make-instance 'rule
                              :name ',name
                              :predicate ,predicate
                              :priority (or ,priority default-priority)
                              :layout ',layout)))

     (push rule *rules*)
     rule))

(defun parse-layout-output-config (configs)
  (dolist (out-config configs)
    (destructuring-bind (type name-predicate &rest options) out-config
      (format t "output details -> type: ~a; pred: ~a; options: ~a~%" type name-predicate (getf options :mode))
    )))

(defun parse-layout-config (config)
  (unless (and (listp config) (> (length config) 2))
    (error "define-layout rule has an invalid format. Check documentation for proper configuration."))
  (destructuring-bind (_ name outputs) config
    (declare (ignore _))
    (format t "Received layout with name: ~a and outputs ~a~%" name (length outputs))
    (parse-layout-output-config outputs)))

(defun parse-rule-config (config)
  (format t "Received rule config: ~a~%" config))

(defun parse-config (config)
  (loop for option in config
        do (progn
             (unless (listp option)
               (error "Config must be a list"))
             (case (first option)
               (define-layout (parse-layout-config option))
               (define-rule (parse-rule-config option))
               (otherwise (format t "Unknown option '~a'~%" (first option)))))))
