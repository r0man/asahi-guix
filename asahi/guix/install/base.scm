(define-module (asahi guix install base)
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
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-bootloader)
                 (targets '("/dev/sda"))))
    (initrd-modules asahi-initrd-modules)
    (kernel-arguments
     '("modprobe.blacklist=radeon"
       ;; Add the 'net.ifnames' argument to prevent network interfaces
       ;; from having really long names. This can cause an issue with
       ;; wpa_supplicant when you try to connect to a wifi network.
       "net.ifnames=0"
       "quiet"))
    (services (cons* %channels-service (operating-system-user-services installation-os)))))

asahi-installation-os
