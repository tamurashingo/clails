(in-package #:cl-user)
(defpackage #:clails/model/impl/dummy
  (:use #:cl)
  (:import-from #:clails/environment
                #:<database-type-dummy>)
  (:import-from #:clails/model/migration
                #:create-table-impl
                #:add-column-impl
                #:add-index-impl
                #:drop-table-impl
                #:drop-column-impl
                #:drop-index-impl))
(in-package #:clails/model/impl/dummy)


(defmethod create-table-impl ((database-type <database-type-dummy>) connection &key table columns constraints)
  (declare (ignore database-type connection table columns constraints))
  nil)

(defmethod add-column-impl ((database-type <database-type-dummy>) connection &key table columns)
  (declare (ignore database-type connection table columns))
  nil)

(defmethod add-index-impl ((database-type <database-type-dummy>) connection &key table index columns)
  (declare (ignore database-type connection table index columns))
  nil)

(defmethod drop-table-impl ((database-type <database-type-dummy>) connection &key table)
  (declare (ignore database-type connection table))
  nil)

(defmethod drop-column-impl ((database-type <database-type-dummy>) connection &key table column)
  (declare (ignore database-type connection table column))
  nil)

(defmethod drop-index-impl ((database-type <database-type-dummy>) connection &key table index)
  (declare (ignore database-type connection table index))
  nil)

