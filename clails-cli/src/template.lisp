(defclass (<template>) ()
  ((path
    :initarg :path
    :reader path
    :type string
    :documentation "Relative path from the project directory where this file is located")
   (template
    :initarg :template
    :reader template
    :type string)))


(defparameter *asd-template*
  (make-instance '<template>
                 :path "/"
                 :template   "(in-package #:cl-user)
(defpackage #:<%= project-name %>-system
  (:use #:asdf #:cl))
(in-package #:<%= project-name %>-system)

(defsystem <%= project-name %>
  :description \"\"
  :version \"0.0.1\"
  :author \"\"
  :license \"\"
  :depends-on (\"clails-cli\"
               \"clails-entity\")
  :components ((:module \"app\"
                :components ((:file \"packages\")))
               (:module \"config\"
                :components ((:file \"packages\")))
               (:module \"db\"
                :components ((:file \"packages\")))))

"))


(defparameter *controller-package-template*
  "(in-package #:cl-user)
(defpackage #:<%= project-name %>-controller
  (:use #:cl))
(in-package #:<%= project-name %>-controller)
")

(defparameter *entity-package-template*
  "(in-package #:cl-user)
(defpackage #:<%= project-name %>-entity
  (:use #:cl
        #:clails-entity))
(in-package #:<%= project-name %>-entity)
")

(defparameter *view-package-template*
  "(in-package #:cl-user)
(defpackage #:<%= project-name %>-view
  (:use #:cl))
(in-package #:<%= project-name %>-view)
")



(defparameter *controller-template*
  "")

(defparameter *entity-template*
  "")

(defparameter *migration-template*
  "")

(defparameter *view-template*
  "")


