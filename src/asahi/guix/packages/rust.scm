(define-module (asahi guix packages rust)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages rust)
  #:use-module (guix download)
  #:use-module (gnu packages rust-apps)
  #:use-module (guix build-system copy)
  #:use-module (guix packages))

(define (rust-bootstrapped-package base-rust version checksum)
  ((@@ (gnu packages rust) rust-bootstrapped-package) base-rust version checksum))

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

(define-public rust-1.69
  (rust-bootstrapped-package
   (@@ (gnu packages rust) rust-1.68)
   "1.69.0" "03zn7kx5bi5mdfsqfccj4h8gd6abm7spj0kjsfxwlv5dcwc9f1gv"))

(define-public rust-src-1.69
  (make-rust-source-package rust-1.69))
