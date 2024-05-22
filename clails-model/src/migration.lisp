(in-package #:cl-user)
(defpackage #:clails-model/migration
  (:use #:cl)
  (:export #:create-column-impl))
(in-package #:clails-model/migration)

(defparameter *tbl* '())

(defclass <database> () ())

(defparameter *db* (make-instance '<database>))

(defun create-column (column-name type not-null-p primary-key-p auto-increment-p)
  (apply #'create-column-impl *db* `(:column-name ,column-name
                                     :type ,type
                                     :not-null-p ,not-null-p
                                     :primary-key-p ,primary-key-p
                                     :auto-increment-p ,auto-increment-p)))

(defgeneric create-column-impl (db &key)
  (:documentation ""))


(defparameter *type-list*
  '(:string :text :integer :float :decimal :datetime :date :time :boolean))


(defun find-table (table)
  (assoc table *tbl*))

(defmacro create-table (table body)
  (when (find-table table)
    (error "~A already defined" table))
  `(setq *tbl* (push (cons ',table ',body) *tbl*)))

(defmacro add-column (table columns)
  (when (null (find-table table))
    (error "~A not defined yet" table))
  `(let ((table-columns (cdr (find-table ',table))))
     (nconc table-columns ',columns)))



(defun migrate ()
  "")


(defun generate-model-class ()
  "")

(defun generate-ddl ()
  "")



(defun all-migration-files ()
  (let* ((basedir (format NIL "~A/*.lisp" *basedir*))
         (allfiles (directory basedir)))
    (loop for file in allfiles
          )))


; (create-table todo
;  ((title :type string)
;   (done :type boolean)))


; (add-column todo
;   ((done-at :type datetime)))


(defun parse-migration (s)
  (when (null s)
    (error "empty"))
  (when (atom s)
    (error "atom:~A" s))
  (let ((op (car s)))
    (cond ((string= "CREATE-TABLE" (symbol-name op))
           (let* ((templ '((id :type :integer
                               :not-null T
                               :primary-key T
                               :auto-increment T)
                           (created-at :type :datetime
                                       :not-null T)
                           (updated-at :type :datetime
                                       :not-null T)))
                  (body (nconc templ (caddr s))))
             (list
              (format NIL "create table ~A ( ~{ ~A~^, ~} )"
                      (cadr s)
                      (parse-create-table body)))))
          ((string= "ADD-COLUMN" (symbol-name op))
           (loop for col in (parse-add-column (caddr s))
                 collect (format NIL "alter table ~A add column ~A" (cadr s) col)))
          (t (error "unknown op:~A" op)))))

(defun parse-create-table (body)
  (when (null body)
    (error "empty body"))
  (loop for col in body
        collect (parse-column col)))


(defun parse-add-column (body)
  (when (null body)
    (error "empty body"))
  (loop for col in body
        collect (parse-column col)))


(defun parse-column (col)
  (when (null col)
    (error "empty column"))
  (let* ((column-name (car col))
         (attr (cdr col))
         (type (getf attr :type))
         (not-null-p (getf attr :not-null))
         (primary-key-p (getf attr :primary-key))
         (auto-increment-p (getf attr :auto-increment)))
    (check-type-valid type)
    (create-column column-name type not-null-p primary-key-p auto-increment-p)))

(defun check-type-valid (type)
  (when (not (find type *type-list*))
    (error "type error: ~A" type)))
