(define-module (asahi guix bootloader m1n1)
  #:use-module (asahi guix build bootloader m1n1)
  #:use-module (asahi guix packages bootloader)
  #:use-module (asahi guix packages guile-xyz)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu packages guile)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix modules))

(define install-m1n1-u-boot-grub
  (with-extensions (list guile-asahi-guix guile-zlib)
    (with-imported-modules (source-module-closure
                            '((asahi guix build bootloader m1n1)))
      #~(lambda (bootloader efi-dir mount-point)
          (use-modules (asahi guix build bootloader m1n1))
          (install-m1n1-u-boot-grub bootloader efi-dir mount-point)))))

(define-public m1n1-u-boot-grub-bootloader
  (efi-bootloader-chain
   grub-efi-removable-bootloader
   #:installer install-m1n1-u-boot-grub
   #:packages (list asahi-linux asahi-m1n1 u-boot-apple-m1)))
