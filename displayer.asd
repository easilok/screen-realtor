;; displayer.asd

(asdf:defsystem #:lp-displayer
  :description "X11 display layout configuratior"
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
   (:file "displayer")))
