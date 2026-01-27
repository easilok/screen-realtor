;;;; config-utils.lisp

(in-package #:screen-realtor)

(defun config-filepaths ()
  "Builds a list of known locations for configuration files"
  `(,(merge-pathnames ".config/screen-realtor/config.disp" (user-homedir-pathname)) "config.disp"))

(defun load-config-file (&optional (file-path "config.disp"))
  "Loads and parses the configuration file into the application"
  (load file-path)
  (setq *rules* (sort-rules *rules*)))

(defun get-cli-option (args option &key default (boolean nil))
  "Gets the provided 'option' value out of 'args' if exists"
  (let ((value default)
        (args (or args '())))
    (when (> (length args) 0)
      ;; loops over every pair of args extracting the matching option value
      (loop for i from 0 to (length args)
            do (when (string= (nth i args) option)
                   (if boolean
                       (setf value t)
                       (if (< (+ i 1) (length args))
                           (setf value (nth (+ i 1) args))
                           (error "No value set for option ~a" option))
                       ))))
    value))

(defun resolve-config-file ()
  "Resolves the path for a configuration file from known locations"
  (loop for c in (config-filepaths)
        do (when (uiop:file-exists-p c)
             (format t "Using config from ~a~%" c)
             (return c))))
