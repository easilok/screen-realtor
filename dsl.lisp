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

(defmacro define-rule (name &key predicate layout)
  "Parses a rule configuration from the DSL configuration file into it's own object instance
   Builds a local list of rules configuration to iterate over as *rules*"
  `(let ((rule (make-instance 'rule
                              :name ',name
                              :predicate ,predicate
                              :layout ',layout)))

     (push rule *rules*)
     rule))
