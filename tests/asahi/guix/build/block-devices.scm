(define-module (tests asahi guix build block-devices)
  #:use-module (asahi guix build block-devices)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define suite "test-asahi-guix-build-block-devices")

(test-begin suite)

(test-skip 1)
(test-assert "list block devices"
  (every block-device? (block-devices)))

(test-end suite)
