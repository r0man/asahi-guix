(hall-description
  (name "asahi-guix")
  (prefix "guile")
  (version "0.1")
  (author "r0man")
  (copyright (2023))
  (synopsis "Asahi Guix")
  (description "Asahi Linux on GNU Guix")
  (home-page "https://github.com/r0man/asahi-guix")
  (license gpl3+)
  (dependencies `())
  (skip ())
  (files (libraries
           ((directory
              "src"
              ((directory
                 "asahi"
                 ((directory
                    "guix"
                    ((directory
                       "services"
                       ((scheme-file "firmware")
                        (scheme-file "channels")
                        (scheme-file "udev")
                        (scheme-file "console-font")))
                     (directory
                       "packages"
                       ((scheme-file "guile-xyz")
                        (unknown-type "defconfig.edge")
                        (scheme-file "crates-io")
                        (scheme-file "firmware")
                        (scheme-file "misc")
                        (scheme-file "audio")
                        (scheme-file "gnuzilla")
                        (scheme-file "jemalloc")
                        (scheme-file "rust")
                        (scheme-file "linux")
                        (scheme-file "bootloader")
                        (scheme-file "gl")
                        (unknown-type "defconfig.main")))
                     (directory
                       "system"
                       ((scheme-file "install")
                        (scheme-file "desktop")
                        (scheme-file "base")))
                     (directory "bootloader" ((scheme-file "m1n1")))
                     (directory
                       "build"
                       ((directory "bootloader" ((scheme-file "m1n1")))
                        (scheme-file "firmware")
                        (scheme-file "block-devices")))
                     (scheme-file "substitutes")
                     (scheme-file "initrd")
                     (scheme-file "channels")
                     (unknown-type "substitutes.asahi-guix.org.pub")
                     (scheme-file "udev")
                     (scheme-file "transformations")))))))))
         (tests ((directory
                   "tests"
                   ((directory
                      "asahi"
                      ((directory
                         "guix"
                         ((directory
                            "build"
                            ((scheme-file "block-devices")))))))))))
         (programs ((directory "scripts" ())))
         (documentation
           ((text-file "ChangeLog")
            (text-file "AUTHORS")
            (text-file "NEWS")
            (text-file "HACKING")
            (text-file "COPYING")
            (symlink "README" "README.org")
            (org-file "NOTES")
            (directory
              "doc"
              ((texi-file "version")
               (text-file "stamp-vti")
               (text-file ".dirstamp")
               (info-file "version")
               (texi-file "ashai-guix")
               (info-file "guile-ashai-guix")))))
         (infrastructure
           ((in-file "pre-inst-env")
            (automake-file "Makefile")
            (autoconf-file "configure")
            (directory
              "build-aux"
              ((tex-file "texinfo")
               (text-file "mdate-sh")
               (scheme-file "test-driver")
               (text-file "install-sh")
               (text-file "missing")))
            (directory
              ".github"
              ((directory
                 "workflows"
                 ((unknown-type "x86_64-linux-gnu.yml")
                  (unknown-type "aarch64-linux-gnu.yml")))
               (unknown-type "FUNDING.yml")))
            (directory
              "share"
              ((directory
                 "systems"
                 ((unknown-type "asahi-guix.tmpl")
                  (unknown-type "asahi-guix-edge.tmpl")))))
            (text-file ".gitignore")
            (text-file ".guix-channel")
            (text-file ".guix-authorizations")
            (scheme-file "hall")))))
