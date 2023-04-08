(define-module (asahi guix install base)
  #:use-module (asahi guix system base)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages firmware)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix services)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu system install)
  #:use-module (gnu system)
  #:export (asahi-installation-os))

(define asahi-installation-os
  (operating-system
    (inherit installation-os)
    (kernel asahi-linux)
    (firmware (list asahi-firmware))
    (kernel-arguments '("debug"))
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/dev/sda"))))
    (initrd asahi-initrd)
    (initrd-modules asahi-initrd-modules)
    (services (cons* %channels-service (operating-system-user-services installation-os)))))

asahi-installation-os
