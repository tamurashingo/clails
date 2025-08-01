(in-package #:cl-user)
(defpackage #:clails-test/controller/base-controller
  (:use #:cl
        #:rove
        #:clails/controller/base-controller)
  (:import-from #:clails/environment
                #:*routing-tables*))

(in-package #:clails-test/controller/base-controller)


(defclass <test-controller> (<base-controller>)
  ())

(defclass <test-detail-controller> (<base-controller>)
  ())

(defclass <test-comment-controller> (<base-controller>)
  ())

(setf *routing-tables*
  '((:path "/"
     :controller "clails-test/controller/base-controller::<test-controller>"
     :type :all)
    (:path "/blog/:blog-id"
     :controller "clails-test/controller/base-controller::<test-detail-controller>"
     :type :one)
    (:path "/blog/:blog-id/comment/:comment-id"
     :controller "clails-test/controller/base-controller::<test-comment-controller>"
     :type :one)))

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






