(define-module (asahi guix services firmware)
  #:use-module (asahi guix build firmware)
  #:use-module (asahi guix packages guile-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (asahi-firmware-service-type
            activate-firmware))

(define-record-type* <asahi-firmware-config>
  asahi-firmware-config make-asahi-firmware-config
  asahi-firmware-config?
  (util-linux asahi-firmware-config-util-linux (default util-linux)))

(define (activate-firmware config)
  (let ((util-linux (asahi-firmware-config-util-linux config)))
    (with-extensions (list guile-asahi-guix)
      (with-imported-modules (source-module-closure
                              '((asahi guix build firmware)
                                (guix build utils)
                                ;; TODO: no code for module (guix cpio)
                                (guix cpio)))
        #~(begin
            (use-modules (asahi guix build firmware))
            (display ":: Asahi Guix: Starting Asahi firmware service ...\n")
            (display "BLOCKID\n")
            (display #$(file-append util-linux "/sbin/blkid"))
            (newline)
            (invoke #$(file-append util-linux "/sbin/blkid"))
            (setup-firmware)
            (display ":: Asahi Guix: Asahi firmware service started.\n")
            #t)))))

(define asahi-firmware-service-type
  (let ((util-linux-package (compose list asahi-firmware-config-util-linux)))
    (service-type
     (name 'asahi-firmware)
     (extensions
      (list (service-extension activation-service-type
                               activate-firmware)
            (service-extension profile-service-type
                               (lambda (config)
                                 (list util-linux)))))
     (description "Service that loads the Apple Silicon firmware at boot time.")
     (default-value (asahi-firmware-config)))))
