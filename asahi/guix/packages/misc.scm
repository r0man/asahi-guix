(define-module (asahi guix packages misc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages bootloader)
  #:use-module (asahi guix packages firmware)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages python)
  #:use-module (guix build-system gnu)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public asahi-scripts
  (package
    (name "asahi-scripts")
    (version "20221220")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/asahi-scripts.git")
             (commit version)))
       (sha256
        (base32 "06a1ixcvnzn9hj1wzfmvvnr9ddgdqqap87b7cf3f92av1a6p6576"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("PREFIX=/usr/local") "PREFIX="))
               (substitute* "asahi-fwextract"
                 (("/usr/share/asahi-scripts/functions.sh")
                  (string-append out "/share/asahi-scripts/functions.sh"))
                 (("python3")
                  (string-append (assoc-ref inputs "python") "/bin/python3")))
               (substitute* "update-grub"
                 (("/usr/share/asahi-scripts/functions.sh")
                  (string-append out "/share/asahi-scripts/functions.sh")))
               (substitute* "update-m1n1"
                 (("/usr/share/asahi-scripts/functions.sh")
                  (string-append out "/share/asahi-scripts/functions.sh"))
                 (("/usr/lib/asahi-boot/")
                  (string-append (assoc-ref inputs "asahi-m1n1") "/libexec/"))
                 (("\\$SOURCE/u-boot-nodtb.bin")
                  (string-append (assoc-ref inputs "u-boot-apple-m1") "/libexec/u-boot-nodtb.bin"))
                 (("/lib/modules/\\*-ARCH/dtbs/\\*.dtb")
                  (string-append (assoc-ref inputs "linux") "/lib/dtbs/apple/*.dtb"))))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "DESTDIR" (assoc-ref outputs "out"))))
         (delete 'check)
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/asahi-fwextract")
                 `("GUIX_PYTHONPATH" ":" prefix
                   (,(getenv "GUIX_PYTHONPATH")))
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "lzfse") "/lib"))))))))))
    (inputs `(("asahi-fwextract" ,asahi-fwextract)
              ("asahi-m1n1" ,asahi-m1n1)
              ("lzfse" ,lzfse)
              ("python" ,python)
              ("u-boot-apple-m1" ,u-boot-apple-m1)
              ("linux" ,asahi-linux)))
    (home-page "https://github.com/AsahiLinux/asahi-scripts")
    (synopsis "Asahi Linux scripts")
    (description "Miscellaneous admin scripts for the Asahi Linux reference distro")
    (license license:expat)))
