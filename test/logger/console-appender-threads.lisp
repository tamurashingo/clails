(defpackage #:clails-test/logger/console-appender-threads
  (:use #:cl
        #:rove
        #:clails/logger))
(in-package #:clails-test/logger/console-appender-threads)

(deftest console-appender-thread-safety-tests
  (testing "Console appender works in main thread"
    (clear-loggers)
    (let ((output (with-output-to-string (*standard-output*)
                    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
                      (register-logger :test :appender appender :level :info)
                      (log-to :test :info "Log from main thread")))))
      (ok (search "Log from main thread" output) "Log output is captured from main thread")))
  
  (testing "Console appender works in spawned thread"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :test :appender appender :level :info)
      (let ((result nil))
        (let ((thread (bt:make-thread
                       (lambda ()
                         (setf result
                               (with-output-to-string (*standard-output*)
                                 (log-to :test :info "Log from spawned thread"))))
                       :name "test-thread")))
          (bt:join-thread thread))
        (ok (search "Log from spawned thread" result) 
            "Log output is captured from spawned thread"))))
  
  (testing "Console appender works with multiple threads"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :test :appender appender :level :info)
      (let ((results (make-array 3 :initial-element nil))
            (threads nil))
        (dotimes (i 3)
          (push (bt:make-thread
                 (let ((index i))
                   (lambda ()
                     (setf (aref results index)
                           (with-output-to-string (*standard-output*)
                             (log-to :test :info (format nil "Log from thread ~A" index))))))
                 :name (format nil "worker-~A" i))
                threads))
        (dolist (thread threads)
          (bt:join-thread thread))
        (dotimes (i 3)
          (ok (search (format nil "Log from thread ~A" i) (aref results i))
              (format nil "Log output captured from thread ~A" i))))))
  
  (testing "Console appender respects *standard-output* binding"
    (clear-loggers)
    (let ((appender (make-console-appender :formatter (make-instance '<text-formatter>))))
      (register-logger :test :appender appender :level :info)
      
      ;; Test that different bindings work correctly
      (let ((output1 (with-output-to-string (*standard-output*)
                       (log-to :test :info "Message 1")))
            (output2 (with-output-to-string (*standard-output*)
                       (log-to :test :info "Message 2"))))
        (ok (search "Message 1" output1) "First message captured")
        (ok (search "Message 2" output2) "Second message captured")
        (ok (not (search "Message 2" output1)) "First output doesn't contain second message")
        (ok (not (search "Message 1" output2)) "Second output doesn't contain first message")))))
