(define-module (asahi guix packages guile-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public guile-asahi-guix
  (let ((commit "4977272e275551cfd2d7fb16554689d4410a1dd9")
        (revision "1"))
    (package
      (name "guile-asahi-guix")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/r0man/asahi-guix.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "16w6j52blv7yn5b32ijb8rdi71xc4wid8nb048r1cmf3skk5s3sp"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf
             automake
             guile-hall
             guix
             pkg-config
             texinfo
             util-linux))
      (inputs (list guile-3.0-latest util-linux))
      (synopsis "Asahi Guix")
      (description "Asahi Linux on GNU Guix")
      (home-page "https://github.com/r0man/asahi-guix")
      (license license:gpl3+))))
