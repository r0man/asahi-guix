(define-module (asahi guix packages guile-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages texinfo)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public guile-asahi-guix
  (let ((commit "04e8abbe61782cdcfae0acf9210d13709af5d3b3")
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
                  "19cnj5pivm23yymxfn6j62r6ax1zrkng2mqdycd072dpb4cfxz0j"))))
      (build-system guile-build-system)
      (inputs (list (lookup-package-input guix "guile")))
      (propagated-inputs (list guix))
      (synopsis "Asahi Guix")
      (description "Asahi Linux on GNU Guix")
      (home-page "https://github.com/r0man/asahi-guix")
      (license license:gpl3+))))
