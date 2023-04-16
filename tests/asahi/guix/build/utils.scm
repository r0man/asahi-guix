(define-module (tests asahi guix build utils)
  #:use-module (asahi guix build utils)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-utils")

(test-begin suite)

(test-equal "read-json-file"
  '(@ ("hello" . "world"))
  (call-with-temporary-output-file
   (lambda (file port)
     (call-with-output-file file
       (lambda (port)
         (format port "{\"hello\": \"world\"}")))
     (read-json-file file))))

(test-assert "write-json-file"
  (call-with-temporary-output-file
   (lambda (file port)
     (let ((data '(@ ("hello" . "world"))))
       (write-json-file file '(@ ("hello" . "world")))
       (equal? data (read-json-file file))))))

(test-end suite)
