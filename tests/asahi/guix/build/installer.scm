(define-module (tests asahi guix build installer)
  #:use-module (asahi guix build installer)
  #:use-module (guix build utils)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-installer")

(test-begin suite)

(test-assert "asahi-installer-build"
  (asahi-installer-build %asahi-installer))

(test-equal "asahi-installer-read-data"
  %asahi-installer
  (call-with-temporary-output-file
   (lambda (file port)
     (asahi-installer-write-data %asahi-installer file)
     (asahi-installer-read-data file))))

(test-assert "asahi-installer-write-data"
  (call-with-temporary-output-file
   (lambda (file port)
     (asahi-installer-write-data %asahi-installer file))))

(test-end suite)
