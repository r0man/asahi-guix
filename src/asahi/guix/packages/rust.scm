(define-module (asahi guix packages rust)
  #:use-module ((gnu packages rust) #:prefix gnu:)
  #:use-module (asahi guix packages jemalloc))

(define-public rust
  (replace-jemalloc gnu:rust))

(define-public rust-src
  (@@ (gnu packages rust) rust-src))
