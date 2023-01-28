(define-module (asahi guix installer)
  #:use-module (asahi guix linux)
  #:use-module (asahi guix packages)
  #:use-module (asahi guix services)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mtools)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (gnu system install)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system)
  #:use-module (guix git-download)
  #:use-module (guix)
  #:use-module (nongnu packages linux)
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

(define asahi-installation-os-edge
  (operating-system
    (inherit asahi-installation-os)
    (kernel asahi-linux-edge)
    (initrd-modules asahi-initrd-modules-edge)))

asahi-installation-os-edge
