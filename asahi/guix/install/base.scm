(define-module (asahi guix install base)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix services channels)
  #:use-module (asahi guix services firmware)
  #:use-module (asahi guix substitutes)
  #:use-module (asahi guix system base)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (gnu system install)
  #:use-module (gnu system)
  #:export (asahi-installation-os))

(define %services
  (modify-services (append (operating-system-user-services installation-os)
                           (list (service asahi-firmware-service-type)
                                 %channels-service))
    (guix-service-type config => (append-substitutes config))))

(define asahi-installation-os
  (operating-system
    (inherit installation-os)
    (kernel asahi-linux)
    (kernel-arguments '("debug"))
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/dev/sda"))))
    (initrd-modules asahi-initrd-modules)
    (services %services)))

asahi-installation-os
