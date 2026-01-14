(in-package #:cl-user)
(defpackage #:clails-test/controller/base-controller
  (:use #:cl
        #:rove
        #:clails/controller/base-controller)
  (:import-from #:clails/environment
                #:*routing-tables*
                #:*project-dir*))

(in-package #:clails-test/controller/base-controller)


(defclass <test-controller> (<base-controller>)
  ())

(defclass <test-detail-controller> (<base-controller>)
  ())

(defclass <test-comment-controller> (<base-controller>)
  ())

(setf *routing-tables*
  '((:path "/"
     :controller "clails-test/controller/base-controller::<test-controller>")
    (:path "/blog/:blog-id"
     :controller "clails-test/controller/base-controller::<test-detail-controller>")
    (:path "/blog/:blog-id/comment/:comment-id"
     :controller "clails-test/controller/base-controller::<test-comment-controller>")))

(setup
  (initialize-routing-tables))


(deftest create-scanner-test
  (testing "without parameter"
    (let ((result (create-scanner-from-uri-path "/")))
      (ok (string= (getf result :scanner) "^/$"))
      (ok (null (getf result :keys))))
    (let ((result (create-scanner-from-uri-path "/users")))
      (ok (string= (getf result :scanner) "^/users$"))
      (ok (null (getf result :keys))))
    (let ((result (create-scanner-from-uri-path "/user/profile")))
      (ok (string= (getf result :scanner) "^/user/profile$"))
      (ok (null (getf result :keys)))))

  (testing "with single parameter"
    (let ((result (create-scanner-from-uri-path "/users/:id")))
      (ok (string= (getf result :scanner) "^/users/([0-9A-Za-z\\-._~%]+)$"))
      (ok (equal (getf result :keys) '("id")))))

  (testing "with double parameter"
    (let ((result (create-scanner-from-uri-path "/blog/:blog-id/comment/:comment-id")))
      (ok (string= (getf result :scanner) "^/blog/([0-9A-Za-z\\-._~%]+)/comment/([0-9A-Za-z\\-._~%]+)$"))
      (ok (equal (getf result :keys) '("blog-id" "comment-id"))))))



(deftest path-controller-test
  (let ((result (clails/controller/base-controller::path-controller "/")))
    (ok result)
    (ok (string= (getf result :controller)
                 "clails-test/controller/base-controller::<test-controller>"))
    (ok (= (length (getf result :parameters)) 0)))

  (let ((result (clails/controller/base-controller::path-controller "/blog/123")))
    (ok result)
    (ok (string= (getf result :controller)
                 "clails-test/controller/base-controller::<test-detail-controller>"))
    (ok (= (length (getf result :parameters))
           1))
    (ok (string= (aref (getf result :parameters) 0)
                 "123")))

  (let ((result (clails/controller/base-controller::path-controller "/blog/abc/comment/def")))
    (ok result)
    (ok (string= (getf result :controller)
                 "clails-test/controller/base-controller::<test-comment-controller>"))
    (ok (= (length (getf result :parameters))
           2))
    (ok (string= (aref (getf result :parameters) 0)
                 "abc"))
    (ok (string= (aref (getf result :parameters) 1)
                 "def"))))


(deftest view-package-resolution-test
  (testing "split-view-path extracts directory parts"
    (ok (equal (clails/controller/base-controller::split-view-path "index.html") nil))
    (ok (equal (clails/controller/base-controller::split-view-path "todo/show.html") '("todo")))
    (ok (equal (clails/controller/base-controller::split-view-path "admin/user/list.html") '("admin" "user"))))

  (testing "make-keyword converts string to keyword"
    (ok (eq (clails/controller/base-controller::make-keyword "testapp/views/package")
            :testapp/views/package))
    (ok (eq (clails/controller/base-controller::make-keyword "testapp/views/todo/package")
            :testapp/views/todo/package)))

  (testing "resolve-view-package resolves package name from view path"
    (let ((clails/environment:*project-name* "testapp"))
      (ok (eq (clails/controller/base-controller::resolve-view-package "index.html")
              :testapp/views/package))
      (ok (eq (clails/controller/base-controller::resolve-view-package "todo/show.html")
              :testapp/views/todo/package))
      (ok (eq (clails/controller/base-controller::resolve-view-package "admin/user/list.html")
              :testapp/views/admin/user/package)))))


(deftest set-view-with-package-test
  (testing "set-view sets view-package slot"
    (let* ((clails/environment:*project-dir* #P"/home/user/projects/testapp/")
           (clails/environment:*project-name* "testapp")
           (controller (make-instance '<web-controller>)))
      (set-view controller "todo/show.html")
      (ok (eq (view-package controller) :testapp/views/todo/package))
      (ok (view controller)))))


(defclass <test-spa-controller> (<base-controller>)
  ())

(defclass <test-static-controller> (<base-controller>)
  ())

(defclass <test-numeric-controller> (<base-controller>)
  ())


(defun generate-wildcard-scanner (route-entry)
  "Generate scanner for wildcard patterns.

   @param route-entry [plist] Route entry
   @return [plist] Scanner and keys
   "
  (let ((path (getf route-entry :path)))
    (cond ((ppcre:scan "\\*$" path)
           `(:scanner ,(ppcre:regex-replace "\\*$" (concatenate 'string "^" path) ".*$")
             :keys nil))
          (t
           (create-scanner-from-uri-path path)))))


(defun generate-numeric-id-scanner (route-entry)
  "Generate scanner for numeric ID patterns.

   @param route-entry [plist] Route entry
   @return [plist] Scanner and keys
   "
  (let ((path (getf route-entry :path)))
    `(:scanner "^/users/([0-9]+)$"
      :keys ("id"))))


(deftest initialize-routing-tables-test
  (testing "Default behavior - only :path and :controller specified"
    (let* ((test-tables '((:path "/test/:id"
                           :controller "test::<test-controller>")))
           (*routing-tables* test-tables))
      (initialize-routing-tables)
      (let ((route (first clails/controller/base-controller::*router*)))
        (ok (string= (getf route :scanner) "^/test/([0-9A-Za-z\\-._~%]+)$"))
        (ok (equal (getf route :keys) '("id"))))))

  (testing ":scanner only (keys should be NIL)"
    (let* ((test-tables '((:path "/spa/*"
                           :controller "test::<spa-controller>"
                           :scanner "^/spa/.*$")))
           (*routing-tables* test-tables))
      (initialize-routing-tables)
      (let ((route (first clails/controller/base-controller::*router*)))
        (ok (string= (getf route :scanner) "^/spa/.*$"))
        (ok (null (getf route :keys))))))

  (testing "Both :scanner and :keys specified"
    (let* ((test-tables '((:path "/static/*"
                           :controller "test::<static-controller>"
                           :scanner "^/static/(.*)$"
                           :keys ("filepath"))))
           (*routing-tables* test-tables))
      (initialize-routing-tables)
      (let ((route (first clails/controller/base-controller::*router*)))
        (ok (string= (getf route :scanner) "^/static/(.*)$"))
        (ok (equal (getf route :keys) '("filepath"))))))

  (testing ":generate-scanner specified"
    (let* ((test-tables `((:path "/api/*"
                           :controller "test::<api-controller>"
                           :generate-scanner ,#'generate-wildcard-scanner)))
           (*routing-tables* test-tables))
      (initialize-routing-tables)
      (let ((route (first clails/controller/base-controller::*router*)))
        (ok (string= (getf route :scanner) "^/api/.*$"))
        (ok (null (getf route :keys))))))

  (testing "Both :scanner and :generate-scanner specified (warning, :scanner prioritized)"
    (let* ((test-tables `((:path "/test"
                           :controller "test::<test-controller>"
                           :scanner "^/test$"
                           :generate-scanner ,#'generate-wildcard-scanner)))
           (*routing-tables* test-tables)
           (warning-output (make-string-output-stream)))
      (let ((*error-output* warning-output))
        (initialize-routing-tables))
      (let ((route (first clails/controller/base-controller::*router*))
            (warning-text (get-output-stream-string warning-output)))
        (ok (string= (getf route :scanner) "^/test$"))
        (ok (ppcre:scan "Warning.*:scanner.*:generate-scanner" warning-text)))))

  (testing "Mixed patterns in *routing-tables*"
    (let* ((test-tables `((:path "/"
                           :controller "test::<root-controller>")
                          (:path "/spa/*"
                           :controller "test::<spa-controller>"
                           :scanner "^/spa/.*$")
                          (:path "/users/:id"
                           :controller "test::<user-controller>"
                           :generate-scanner ,#'generate-numeric-id-scanner)))
           (*routing-tables* test-tables))
      (initialize-routing-tables)
      (let ((routes clails/controller/base-controller::*router*))
        (ok (= (length routes) 3))
        (ok (string= (getf (first routes) :scanner) "^/$"))
        (ok (string= (getf (second routes) :scanner) "^/spa/.*$"))
        (ok (string= (getf (third routes) :scanner) "^/users/([0-9]+)$"))))))


(deftest initialize-routing-tables-error-test
  (testing ":scanner is not a string"
    (let* ((test-tables '((:path "/test"
                           :controller "test::<test-controller>"
                           :scanner 123)))
           (*routing-tables* test-tables))
      (ok (signals (initialize-routing-tables) 'error))))

  (testing ":generate-scanner is not a function"
    (let* ((test-tables '((:path "/test"
                           :controller "test::<test-controller>"
                           :generate-scanner not-a-function)))
           (*routing-tables* test-tables))
      (ok (signals (initialize-routing-tables) 'error))))

  (testing ":generate-scanner returns invalid format"
    (let* ((invalid-generator (lambda (route) "invalid"))
           (test-tables `((:path "/test"
                           :controller "test::<test-controller>"
                           :generate-scanner ,invalid-generator)))
           (*routing-tables* test-tables))
      (ok (signals (initialize-routing-tables) 'error)))))


(deftest path-controller-with-custom-scanner-test
  (testing "Catch-all route matching"
    (let* ((test-tables '((:path "/spa/*"
                           :controller "clails-test/controller/base-controller::<test-spa-controller>"
                           :scanner "^/spa/.*$")))
           (*routing-tables* test-tables))
      (initialize-routing-tables)
      (let ((result1 (clails/controller/base-controller::path-controller "/spa/"))
            (result2 (clails/controller/base-controller::path-controller "/spa/home"))
            (result3 (clails/controller/base-controller::path-controller "/spa/users/123")))
        (ok result1)
        (ok (string= (getf result1 :controller)
                     "clails-test/controller/base-controller::<test-spa-controller>"))
        (ok result2)
        (ok (string= (getf result2 :controller)
                     "clails-test/controller/base-controller::<test-spa-controller>"))
        (ok result3)
        (ok (string= (getf result3 :controller)
                     "clails-test/controller/base-controller::<test-spa-controller>")))))

  (testing "Static file route with parameter"
    (let* ((test-tables '((:path "/static/*"
                           :controller "clails-test/controller/base-controller::<test-static-controller>"
                           :scanner "^/static/(.*)$"
                           :keys ("filepath"))))
           (*routing-tables* test-tables))
      (initialize-routing-tables)
      (let ((result (clails/controller/base-controller::path-controller "/static/images/logo.png")))
        (ok result)
        (ok (string= (getf result :controller)
                     "clails-test/controller/base-controller::<test-static-controller>"))
        (ok (= (length (getf result :parameters)) 1))
        (ok (string= (aref (getf result :parameters) 0) "images/logo.png")))))

  (testing "Numeric ID only route"
    (let* ((test-tables `((:path "/users/:id"
                           :controller "clails-test/controller/base-controller::<test-numeric-controller>"
                           :generate-scanner ,#'generate-numeric-id-scanner)))
           (*routing-tables* test-tables))
      (initialize-routing-tables)
      (let ((result1 (clails/controller/base-controller::path-controller "/users/123"))
            (result2 (clails/controller/base-controller::path-controller "/users/abc")))
        (ok result1)
        (ok (string= (getf result1 :controller)
                     "clails-test/controller/base-controller::<test-numeric-controller>"))
        (ok (= (length (getf result1 :parameters)) 1))
        (ok (string= (aref (getf result1 :parameters) 0) "123"))
        (ok (null result2))))))


