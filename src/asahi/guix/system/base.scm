(define-module (asahi guix system base)
  #:use-module (asahi guix bootloader m1n1)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages linux)
  #:use-module (asahi guix services console-font)
  #:use-module (asahi guix services firmware)
  #:use-module (asahi guix substitutes)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system nss)
  #:use-module (gnu system shadow)
  #:use-module (gnu system)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (ice-9 optargs)
  #:export (asahi-operating-system))

(define %kernel-arguments
  (append '("net.ifnames=0") %default-kernel-arguments))

(define %keyboard-options
  '("caps:ctrl_modifier"
    "terminate:ctrl_alt_bksp"))

(define %keyboard-layout
  (keyboard-layout "us" #:options %keyboard-options))

(define (make-esp-file-system uuid-str)
  (file-system
    (device (uuid uuid-str 'fat32))
    (mount-point "/boot/efi")
    (needed-for-boot? #t)
    (type "vfat")))

(define (make-root-file-system label)
  (file-system
    (device (file-system-label label))
    (mount-point "/")
    (needed-for-boot? #t)
    (type "ext4")))

(define* (make-file-systems #:key esp-uuid root-label)
  (append (filter file-system?
                  (list (when (string? esp-uuid)
                          (make-esp-file-system esp-uuid))
                        (when (string? root-label)
                          (make-root-file-system root-label))))
          %base-file-systems))

(define %packages
  (cons* e2fsprogs
         network-manager
         nss-certs
         %base-packages))

(define %services
  (modify-services (cons* (service network-manager-service-type)
                          (service openssh-service-type
                                   (openssh-configuration
                                    (openssh openssh-sans-x)))
                          (service wpa-supplicant-service-type)
                          (append %base-services
                                  (list (service asahi-firmware-service-type))))
    (console-font-service-type config => (console-font-terminus config))
    (guix-service-type config => (append-substitutes config))))

(define %users
  (cons (user-account
         (name "guest")
         (comment "Guest")
         (group "users")
         (supplementary-groups '("wheel" "audio" "netdev" "video")))
        %base-user-accounts))

(define* (asahi-operating-system
          #:key
          (esp-uuid #f)
          (host-name "asahi-guix")
          (initrd-modules asahi-initrd-modules)
          (kernel asahi-linux)
          (keyboard-layout %keyboard-layout)
          (locale "en_US.utf8")
          (root-label "asahi-guix-root")
          (timezone "Europe/Berlin"))
  (operating-system
    (host-name host-name)
    (locale locale)
    (timezone timezone)
    (keyboard-layout keyboard-layout)
    (bootloader (bootloader-configuration
                 (bootloader m1n1-u-boot-grub-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout keyboard-layout)))
    (kernel kernel)
    (kernel-arguments %kernel-arguments)
    (initrd-modules initrd-modules)
    (file-systems (make-file-systems
                   #:esp-uuid esp-uuid
                   #:root-label root-label))
    (packages %packages)
    (services %services)
    (users %users)))
