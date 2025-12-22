;;;; system.lisp

(in-package #:lp-displayer)

(defun parse-xrandr ()
  "Parser for the xrandr information"
  (let ((lines (uiop:run-program '("xrandr" "--query") :output :lines)))
    (loop with outputs = '()
          for line in lines
          do (when (or (search " connected" line)
                       (search "disconnected" line))
               (let ((name (subseq line 0 (position #\Space line))))
                 (push (make-instance 'output
                       :name name
                       :connected-p (not (null (search " connected" line)))
                       :modes nil
                       :current nil)
                       outputs)))
          finally (return outputs))))

(defun current-wifi ()
  "Parser for the current wifi information"
  (let ((lines (uiop:run-program '("nmcli" "-t" "-f" "ACTIVE,SSID" "dev" "wifi") :output :lines)))
    (loop for line in lines
          when (uiop:string-prefix-p "yes:" line)
            return (subseq line 4)
          finally (return nil))))

(defun xrandr-build-double-value (value)
  "Helper function for building xrandr options with two values (like 'width x height')
   Accepts either a list of values or a string"
  (if (stringp value)
      value
      (format nil "~Ax~A"
              (first value)
              (second value))))

(defun xrandr-command (resolved state)
  "xrandr command builder based on current layout outputs"
  (let ((used (mapcar #'cdr resolved)))
    (append
     '("xrandr")
     (loop for (out-config . out) in resolved append
           (let ((options (list
                           "--output" (output-name out)
                           "--mode"   (xrandr-build-double-value (out-config-mode out-config))
                           "--pos"    (xrandr-build-double-value (out-config-position out-config)))))
             (if (out-config-primary-p out-config)
                 (append options '("--primary"))
                 options)))
     (loop for out in (system-outputs state)
           unless (member out used)
             append (list "--output" (output-name out) "--off")))))


(defun run-command (cmd)
  "Executes a shell command safely providing proper error handling information"
  (multiple-value-bind (out err code)
      (uiop:run-program cmd
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (unless (zerop code)
      (error "Command failed: ~{~A~^ ~}~%~A" cmd err))
    code))
