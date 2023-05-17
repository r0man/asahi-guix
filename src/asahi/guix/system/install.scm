(define-module (asahi guix system install)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix services channels)
  #:use-module (asahi guix services console-font)
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
  #:export (asahi-installation-operating-system))

(define %services
  (modify-services (append (operating-system-user-services installation-os)
                           (list %asahi-channels-service
                                 (service asahi-firmware-service-type)))
    (console-font-service-type config => (console-font-terminus config))
    (guix-service-type config => (append-substitutes config))))

(define asahi-installation-operating-system
  (operating-system
    (inherit installation-os)
    (kernel asahi-linux)
    (kernel-arguments '("debug"))
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/dev/sda"))))
    (initrd-modules asahi-initrd-modules)
    (services %services)))

asahi-installation-operating-system
