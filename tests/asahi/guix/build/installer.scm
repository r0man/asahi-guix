(define-module (tests asahi guix build installer)
  #:use-module (asahi guix build installer)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-installer")

(test-begin suite)

(test-assert "asahi-installer-os-read-data"
  (call-with-temporary-output-file
   (lambda (file port)
     (asahi-installer-os-write-data file 1))))

(test-assert "asahi-installer-os-write-data"
  (call-with-temporary-output-file
   (lambda (file port)
     (asahi-installer-os-write-data file 1))))

(test-end suite)
