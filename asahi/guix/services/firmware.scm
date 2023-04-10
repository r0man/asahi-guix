(define-module (asahi guix services firmware)
  #:use-module (asahi guix build firmware)
  #:use-module (asahi guix packages guile-xyz)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:export (asahi-firmware-service-type))

(define (activate _)
  (with-extensions (list guile-asahi-guix)
    (with-imported-modules (source-module-closure
                            '((asahi guix build firmware)))
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
    (list (service-extension activation-service-type activate)))
   (description "Service that loads the Apple Silicon firmware at boot time.")
   (default-value #f)))
