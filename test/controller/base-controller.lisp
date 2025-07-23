(in-package #:cl-user)
(defpackage #:clails-test/controller/base-controller
  (:use #:cl
        #:rove
        #:clails/controller/base-controller))

(in-package #:clails-test/controller/base-controller)


(deftest create-scanner-test
  (testing "without parameter"
    (let ((result (create-scanner-from-uri-path "/")))
      (ok (string= (getf result :scanner) "^/$"))
      (ok (null (getf result :params))))
    (let ((result (create-scanner-from-uri-path "/users")))
      (ok (string= (getf result :scanner) "^/users$"))
      (ok (null (getf result :params))))
    (let ((result (create-scanner-from-uri-path "/user/profile")))
      (ok (string= (getf result :scanner) "^/user/profile$"))
      (ok (null (getf result :params)))))

  (testing "with single parameter"
    (let ((result (create-scanner-from-uri-path "/users/:id")))
      (ok (string= (getf result :scanner) "^/users/([0-9A-Za-z\\-._~%]+)$"))
      (ok (equal (getf result :params) '(:id)))))

  (testing "with double parameter"
    (let ((result (create-scanner-from-uri-path "/blog/:blog-id/comment/:comment-id")))
      (ok (string= (getf result :scanner) "^/blog/([0-9A-Za-z\\-._~%]+)/comment/([0-9A-Za-z\\-._~%]+)$"))
      (ok (equal (getf result :params) '(:blog-id :comment-id))))))


