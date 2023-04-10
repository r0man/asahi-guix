(define-module (asahi guix services firmware)
  #:use-module (asahi guix build firmware)
  #:use-module (asahi guix packages guile-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:export (asahi-firmware-service-type
            activate-firmware))

(define (activate-firmware _)
  (with-extensions (list guile-asahi-guix)
    (with-imported-modules (source-module-closure
                            '((asahi guix build firmware)
                              (guix build utils)
                              ;; TODO: no code for module (guix cpio)
                              (guix cpio)))
      #~(begin
          (use-modules (asahi guix build firmware))
          (display ":: Asahi Guix: Starting Asahi firmware service ...\n")
          (invoke "blkid")
          (setup-firmware)
          (display ":: Asahi Guix: Asahi firmware service started.\n")
          #t))))

(define asahi-firmware-service-type
  (service-type
   (name 'asahi-firmware)
   (extensions
    (list (service-extension activation-service-type
                             activate-firmware)
          (service-extension profile-service-type
                             (lambda (config)
                               (list util-linux)))))
   (description "Service that loads the Apple Silicon firmware at boot time.")
   (default-value #f)))
