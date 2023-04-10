(define-module (asahi guix services firmware)
  #:use-module (asahi guix packages guile-xyz)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (guix modules)
  #:export (asahi-firmware-service-type))

(define (pre-mount _)
  (with-extensions (list guile-asahi-guix)
    (with-imported-modules (source-module-closure
                            '((asahi guix build firmware)
                              (asahi guix build cpio)
                              (gnu build activation)
                              (gnu build file-systems)
                              (gnu system uuid)
                              (guix build syscalls)
                              (guix build utils)
                              (ice-9 pretty-print)
                              (ice-9 textual-ports)
                              (srfi srfi-1)
                              (gnu build activation)
                              (guix build utils)
                              (guix cpio)))
      #~(begin
          (use-modules (asahi guix build firmware))
          (display ":: Asahi Guix: Starting Asahi firmware service ...\n")
          (setup-firmware)
          (display ":: Asahi Guix: Asahi firmware service started.\n")
          #t))))

(define asahi-firmware-service-type
  (service-type
   (name 'asahi-firmware)
   (extensions
    (list (service-extension activation-service-type pre-mount)))
   (description "Service that loads the Apple Silicon firmware at boot time.")
   (default-value #f)))
