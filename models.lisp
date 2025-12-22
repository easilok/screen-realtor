;;;; models.lisp

(in-package #:lp-displayer)

(defclass output ()
  ;; (:documentation "Model for a system display output information and state")
  ((name        :initarg :name        :reader output-name)
   (connected-p :initarg :connected-p :reader output-connected-p)
   (modes       :initarg :modes       :reader output-modes)
   (current     :initarg :current     :reader output-current)))

(defclass system-state ()
  ;; (:documentation "Model that defines the current system state with relevant information for setting layouts")
  ((outputs   :initarg :outputs   :reader system-outputs)
   (wifi-ssid :initarg :wifi-ssid :reader system-wifi)))

(defclass output-configuration ()
  ;; (:documentation "Model for a user prefered output configuration based on the selector")
  ((selector  :initarg :selector  :reader out-config-selector)
   (mode      :initarg :mode      :reader out-config-mode)
   (position  :initarg :position  :reader out-config-position)
   (primary-p :initarg :primary-p :initform nil :reader out-config-primary-p)))

(defclass layout ()
  ;; (:documentation "Model for a named layout for a group of display output configurations")
  ((name    :initarg :name    :reader layout-name)
   (outputs :initarg :outputs :reader output-configurations)))

(defclass rule ()
  ;; (:documentation "Model for a rule that triggers a specific display output layout")
  ((name      :initarg :name :reader rule-name)
   (predicate :initarg :predicate :reader rule-predicate)
   (layout    :initarg :layout :reader rule-layout)))

(defun name= (string)
  "Helper selector for getting an output with the specified name"
  (lambda (output)
    (string= (output-name output) string)))

(defun not-name= (string)
  "Helper selector for getting any output that do not match the specified name"
  (lambda (output)
    (and (output-connected-p output)
         (not (string= (output-name output) string)))))

(defun wifi= (ssid)
  "Helper for comparing the current wifi name"
  (lambda (state) (string= (system-wifi state) ssid)))

(defun no-wifi ()
  "Helper for checking if system is not connected to an wifi"
  (lambda (state) (null (system-wifi state))))
