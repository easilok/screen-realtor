;; realtor.asd

(asdf:defsystem #:screen-realtor
  :description "X11 screen layout configuratior"
  :author "Luis Pereira"
  :license  "WTFPL"
  :version "0.0.1"
  :serial t
  :depends-on (:slynk)
  :pathname "./"
  :components
  ((:file "package")
   (:file "utils")
   (:file "conditions")
   (:file "models")
   (:file "system")
   (:file "config-utils")
   (:file "dsl")
   (:file "realtor")))
