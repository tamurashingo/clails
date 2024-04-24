(in-package #:cl-user)

(defpackage #:clails-cli-system
  (:use #:asdf #:cl))

(in-package #:clails-cli-system)

(defsystem clails-cli
  :components ((:module "src"
                :components
                ((:file "main")))))
