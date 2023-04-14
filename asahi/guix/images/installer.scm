(define-module (asahi guix images installer)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (gnu image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages certs)
  #:use-module (guix platforms arm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (srfi srfi-26)
  #:export (installer-barebones-os
            installer-image-type
            installer-barebones-raw-image))

(define installer-barebones-os
  (operating-system
    (host-name "vignemale")
    (timezone "Europe/Paris")
    (locale "en_US.utf8")
    (bootloader (bootloader-configuration
                 (bootloader u-boot-pine64-lts-bootloader)
                 (targets '("/dev/vda"))))
    (initrd-modules '())
    (kernel linux-libre-arm64-generic)
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (services (cons*
               (service agetty-service-type
                        (agetty-configuration
                         (extra-options '("-L")) ; no carrier detect
                         (baud-rate "115200")
                         (term "vt100")
                         (tty "ttyS0")))
               (service dhcp-client-service-type)
               (service ntp-service-type)
               %base-services))
    (packages (cons nss-certs %base-packages))))

(define installer-image-type
  (image-type
   (name 'installer-raw)
   (constructor (lambda (os)
                  (image
                   (inherit (raw-with-offset-disk-image))
                   (operating-system os)
                   (platform aarch64-linux))))))

(define installer-barebones-raw-image
  (image
   (inherit
    (os+platform->image installer-barebones-os aarch64-linux #:type installer-image-type))
   (name 'installer-barebones-raw-image)))

installer-barebones-raw-image
