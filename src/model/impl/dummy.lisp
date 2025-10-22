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
  "Dummy implementation of create-table for testing.
   
   Does nothing and returns nil.
   
   @param database-type [<database-type-dummy>] Dummy database type
   @param connection [t] Connection (ignored)
   @param table [string] Table name (ignored)
   @param columns [list] Column specifications (ignored)
   @param constraints [list] Constraints (ignored)
   @return [nil] Always returns nil
   "
  (declare (ignore database-type connection table columns constraints))
  nil)

(defmethod add-column-impl ((database-type <database-type-dummy>) connection &key table columns)
  "Dummy implementation of add-column for testing.
   
   Does nothing and returns nil.
   
   @param database-type [<database-type-dummy>] Dummy database type
   @param connection [t] Connection (ignored)
   @param table [string] Table name (ignored)
   @param columns [list] Column specifications (ignored)
   @return [nil] Always returns nil
   "
  (declare (ignore database-type connection table columns))
  nil)

(defmethod add-index-impl ((database-type <database-type-dummy>) connection &key table index columns)
  "Dummy implementation of add-index for testing.
   
   Does nothing and returns nil.
   
   @param database-type [<database-type-dummy>] Dummy database type
   @param connection [t] Connection (ignored)
   @param table [string] Table name (ignored)
   @param index [string] Index name (ignored)
   @param columns [list] Column names (ignored)
   @return [nil] Always returns nil
   "
  (declare (ignore database-type connection table index columns))
  nil)

(defmethod drop-table-impl ((database-type <database-type-dummy>) connection &key table)
  "Dummy implementation of drop-table for testing.
   
   Does nothing and returns nil.
   
   @param database-type [<database-type-dummy>] Dummy database type
   @param connection [t] Connection (ignored)
   @param table [string] Table name (ignored)
   @return [nil] Always returns nil
   "
  (declare (ignore database-type connection table))
  nil)

(defmethod drop-column-impl ((database-type <database-type-dummy>) connection &key table column)
  "Dummy implementation of drop-column for testing.
   
   Does nothing and returns nil.
   
   @param database-type [<database-type-dummy>] Dummy database type
   @param connection [t] Connection (ignored)
   @param table [string] Table name (ignored)
   @param column [string] Column name (ignored)
   @return [nil] Always returns nil
   "
  (declare (ignore database-type connection table column))
  nil)

(defmethod drop-index-impl ((database-type <database-type-dummy>) connection &key table index)
  "Dummy implementation of drop-index for testing.
   
   Does nothing and returns nil.
   
   @param database-type [<database-type-dummy>] Dummy database type
   @param connection [t] Connection (ignored)
   @param table [string] Table name (ignored)
   @param index [string] Index name (ignored)
   @return [nil] Always returns nil
   "
  (declare (ignore database-type connection table index))
  nil)

