(define-module (asahi guix packages rust)
  #:use-module (asahi guix packages jemalloc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages rust)
  #:use-module (guix download)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix build-system copy)
  #:use-module (guix packages))

(define (rust-bootstrapped-package base-rust version checksum)
  (replace-jemalloc ((@@ (gnu packages rust) rust-bootstrapped-package)
                     base-rust version checksum)))

(define (make-rust-source-package rust)
  (package
    (inherit rust)
    (name "rust-src")
    (build-system copy-build-system)
    (native-inputs '())
    (inputs '())
    (native-search-paths '())
    (outputs '("out"))
    (arguments
     `(#:install-plan
       '(("library" "lib/rustlib/src/rust/library")
         ("src" "lib/rustlib/src/rust/src"))))
    (synopsis "Source code for the Rust standard library")
    (description "This package provide source code for the Rust standard
library, only use by rust-analyzer, make rust-analyzer out of the box.")))

(define-public rust-1.66
  (rust-bootstrapped-package
   (@@ (gnu packages rust) rust-1.65)
   "1.66.1" "1fjr94gsicsxd2ypz4zm8aad1zdbiccr7qjfbmq8f8f7jhx96g2v"))

(define-public rust-src-1.66
  (make-rust-source-package rust-1.66))

(define-public rust-src-1.62
  (hidden-package
   (package
     (inherit (@@ (gnu packages rust) rust-1.62))
     (name "rust-src")
     (build-system copy-build-system)
     (native-inputs '())
     (inputs '())
     (native-search-paths '())
     (outputs '("out"))
     (arguments
      `(#:install-plan
        '(("library" "lib/rustlib/src/rust/library")
          ("src" "lib/rustlib/src/rust/src"))))
     (synopsis "Source code for the Rust standard library")
     (description "This package provide source code for the Rust standard
library, only use by rust-analyzer, make rust-analyzer out of the box."))))
