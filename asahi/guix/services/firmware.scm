(define-module (asahi guix services firmware)
  #:use-module (asahi guix build firmware)
  #:use-module (asahi guix packages guile-xyz)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages linux)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (asahi-firmware-service-type))

(define-record-type* <asahi-firmware-config>
  asahi-firmware-config make-asahi-firmware-config
  asahi-firmware-config?
  (cpio asahi-firmware-config-cpio (default cpio))
  (util-linux asahi-firmware-config-util-linux (default util-linux)))

(define (activate-firmware config)
  (let ((cpio (asahi-firmware-config-cpio config))
        (util-linux (asahi-firmware-config-util-linux config)))
    (with-imported-modules (source-module-closure '((asahi guix build firmware)))
      #~(begin
          (use-modules (asahi guix build firmware))
          (setup-firmware
           (firmware
            (cpio #$(file-append cpio "/bin/cpio"))
            (blkid #$(file-append util-linux "/sbin/blkid"))))
          #t))))

(define asahi-firmware-service-type
  (let ((util-linux-package (compose list asahi-firmware-config-util-linux)))
    (service-type
     (name 'asahi-firmware)
     (extensions
      (list (service-extension activation-service-type activate-firmware)
            (service-extension profile-service-type util-linux-package)))
     (description "Service that loads the Apple Silicon firmware at boot time.")
     (default-value (asahi-firmware-config)))))
