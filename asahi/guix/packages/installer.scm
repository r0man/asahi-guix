(define-module (asahi guix packages installer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix images installer)
  #:use-module (asahi guix packages bootloader)
  #:use-module (asahi guix packages guile-xyz)
  #:use-module (asahi guix system base)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages package-management)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix modules)
  #:use-module (guix store)
  #:use-module (guix packages))

(define asahi-operating-system-drv
  (with-store %store
    (run-with-store %store
      (lower-object (asahi-operating-system)))))

(define-public asahi-installer-image-base
  (package
    (name "asahi-installer-image-base")
    (version "0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (use-modules (ice-9 pretty-print))
          (let ((coreutils (assoc-ref %build-inputs "coreutils"))
                (grub (assoc-ref %build-inputs "grub"))
                (system (assoc-ref %build-inputs "_"))
                (out (assoc-ref %outputs "out")))
            (setenv "PATH" (string-append coreutils "/bin"))
            (copy-recursively (string-append system "/profile") out)))))
    (native-inputs (list asahi-operating-system-drv
                         coreutils
                         grub))
    (home-page "https://github.com/r0man/asahi-guix")
    (synopsis "Ashai Installer UEFI image.")
    (description
     "This package provieds the Ashai Installer UEFI image zip file.")
    (license license:bsd-3)))

(define-public asahi-installer-image-uefi
  (package
    (name "asahi-installer-image-uefi")
    (version "0.1")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      (with-extensions (list guile-asahi-guix)
        (with-imported-modules (source-module-closure
                                '((asahi guix build bootloader m1n1)
                                  (guix build utils)))
          #~(begin
              (use-modules (asahi guix build bootloader m1n1))
              (use-modules (guix build utils))
              (use-modules (ice-9 pretty-print))
              (let ((asahi-m1n1 (assoc-ref %build-inputs "asahi-m1n1"))
                    (coreutils (assoc-ref %build-inputs "coreutils"))
                    (out (assoc-ref %outputs "out")))
                (setenv "PATH" (string-append coreutils "/bin"))
                (pretty-print asahi-m1n1)
                (mkdir-p (string-append out "/esp/m1n1/boot.bin"))))))))
    (native-inputs (list asahi-m1n1 coreutils))
    (home-page "https://github.com/r0man/asahi-guix")
    (synopsis "Ashai Installer UEFI image.")
    (description
     "This package provieds the Ashai Installer UEFI image zip file.")
    (license license:bsd-3)))
