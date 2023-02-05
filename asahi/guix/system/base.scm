(define-module (asahi guix system base)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages)
  #:use-module (asahi guix transformations)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services)
  #:use-module (gnu system accounts)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system nss)
  #:use-module (gnu system shadow)
  #:use-module (gnu system)
  #:use-module (guix packages)
  #:export (asahi-operating-system))

(define %kernel-arguments
  (append '("net.ifnames=0") %default-kernel-arguments))

(define %keyboard-options
  '("caps:ctrl_modifier"
    "terminate:ctrl_alt_bksp"))

(define %keyboard-layout
  (keyboard-layout "us" #:options %keyboard-options))

(define %file-systems
  (cons (file-system
          (device (file-system-label "my-root"))
          (mount-point "/")
          (type "ext4"))
        %base-file-systems))

(define %packages
  (append (map specification->package
               '("e2fsprogs"
                 "nss-certs"))
          %base-packages))

(define %services
  (cons* (service dhcp-client-service-type)
         (service openssh-service-type
                  (openssh-configuration
                   (openssh openssh-sans-x)))
         %base-services))

(define %users
  (cons (user-account
         (name "alice")
         (comment "Bob's sister")
         (group "users")
         (supplementary-groups '("wheel" "audio" "video")))
        %base-user-accounts))

(define asahi-operating-system
  (operating-system
    (host-name "asahi-base")
    (locale "en_US.utf8")
    (timezone "Europe/Berlin")
    (keyboard-layout %keyboard-layout)
    (bootloader (bootloader-configuration
                 (bootloader grub-efi-removable-bootloader)
                 (targets (list "/boot/efi"))
                 (keyboard-layout keyboard-layout)))
    (kernel (replace-jemalloc asahi-linux))
    (kernel-arguments %kernel-arguments)
    (initrd-modules asahi-initrd-modules)
    (firmware (list asahi-firmware))
    (file-systems %file-systems)
    (packages %packages)
    (services %services)
    (users %users)))

asahi-operating-system
