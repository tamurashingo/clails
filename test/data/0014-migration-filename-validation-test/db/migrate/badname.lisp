(in-package #:clails-test/model/db/migration-filename-validation)

;; Intentionally does not follow the <timestamp>-<name>.lisp / <timestamp>_<name>.lisp
;; naming convention, to exercise db-migrate's filename validation warning.
(defmigration "badname-migration"
  (:up #'(lambda (conn)
           (create-table conn :table "gadgets"
                              :columns '(("name" :type :string
                                                 :not-null T))))
   :down #'(lambda (conn)
             (drop-table conn :table "gadgets"))))
