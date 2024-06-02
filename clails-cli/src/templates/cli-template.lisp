(in-package #:cl-user)
(defpackage #:clails-cli/templates/cli-template
  (:use #:cl)
  (:import-from #:clails-cli/template
                #:<template>)
  (:export #:cli-main-template))
(in-package #:clails-cli/templates/cli-template)


(defparameter cli-main-template
  (make-instance '<template>
                 :path "/cli"
                 :template "(in-package #:cl-user)
(defpackage <%= (@ project-name ) %>-cli
  (:use #:cl)
  (:import-from #:clails-cli/model
                #:generate-model-exp
                #:generate-migration-exp)
  (:import-from #:<%= (@ project-name ) %>
                #:*clails-project*
                #:*clails-directory*)
  (:export :db/create
           :generate/model
           :generate/migration))
(in-package <%= (@ project-name ) %>-cli)


(defun db/create (&key env)
  (let ((envs (if env `(,env)
                      '(:develop :test))))
    (loop for e in envs
          do (<%= (@ project-name ) %>-config:db/create e))))


(defun generate/model (model-name body &key no-migration)
  (generate-model-exp model-name *clails-project* *clails-directory*)
  (when (not no-migration)
    (generate-migration-exp model-name *clails-directory* :type :create :body body)))

(defun generate/migration (model-name &key type body)
  (when (not (or (eq type :create)
                 (eq type :add-column)))
    (error \"type not supported: ~A\" type))
  (generate-migration-exp model-name *clails-directory* :type type :body body))

"))

