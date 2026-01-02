;;;; displayer.lisp

(in-package #:lp-displayer)

(defparameter *layouts* (make-hash-table))
(defparameter *rules* '())

(defun collect-system-state ()
  "Parses current system state information"
  (make-instance 'system-state
   :outputs (parse-xrandr)
   :wifi-ssid (current-wifi)))

(defun resolve-layout (layout state)
  "Validates the possibility of applying the selected layout and returns its output configurations"
  (loop for out-config in (output-configurations layout)
        collect
        (let* ((matches (remove-if-not (out-config-selector out-config)
                                       (system-outputs state))))
          (cond
            ((null matches) (error 'no-output-match :selector (out-config-selector out-config)))
            ((> (length matches) 1) (error 'ambiguous-output-match
                                           :selector (out-config-selector out-config)
                                           :matches matches))
            (t (cons out-config (first matches)))))))

(defun apply-rules (rules state)
  "Loop for configured rules to check the one that matches the current system state"
  (loop for rule in rules
        when (funcall (rule-predicate rule) state)
          do (let ((layout (gethash (rule-layout rule) *layouts*)))
               (when layout
                 ;; Ensure a layout is only applied if there is enough connected outputs
                 (if (<= (length (output-configurations layout))
                         (length (system-outputs-connected state)))
                     (return (rule-layout rule))
                     (format t "Skipping layout '~a' due to insufficient available outputs.~%" layout))))
        finally (error "No matching rule")))

(defun apply-layout (layout state &key (dry-run nil))
  "Applies a specific layout to the current system"
  (format t "Trying to apply layout named ~a (type: ~a)~%" layout (type-of layout))
  (let* ((layout-key (ensure-symbol layout))
         (layout (gethash layout-key *layouts*)))
    (unless layout
      (format t "No layout named ~a~%" layout)
      (return-from apply-layout))
    (let* ((resolved (resolve-layout layout state))
           (cmd (xrandr-command resolved state)))
      (if dry-run
          cmd
          (run-command cmd)))))

(defun main (&optional args)
  "Application entrypoint. Parses rules and layouts configuration and user commands."
  (let* ((args (or args (uiop:command-line-arguments)))
         (config-file (get-cli-option args
                                      "--config"
                                      :default (resolve-config-file)))
         (layout (get-cli-option args "--layout"))
         (dry-run (get-cli-option args "--dry-run" :boolean t))
         (repl (get-cli-option args "--repl" :boolean t))
         (system-state (collect-system-state)))

    ;; Load config in dsl language
    (load-config-file config-file)


    ;; Apply provided layout or select one from rules
    (unless repl
      (if layout
          (apply-layout layout system-state :dry-run dry-run)
          (apply-layout (apply-rules *rules* system-state) system-state :dry-run dry-run))
      (sb-ext:exit))

    (when repl
        (slynk:create-server :port 4009 :dont-close t)
        (handler-case
            (loop (sleep 1))
          (sb-sys:interactive-interrupt ()
            (format t "~%Received interrupt, stopping server...~%"))))))
