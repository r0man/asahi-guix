(define-module (asahi guix packages rust)
  #:use-module ((gnu packages rust) #:prefix gnu:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:export (replace-rust))

(define-public rust
  (package/inherit gnu:rust
    (name "rust")
    (arguments
     (substitute-keyword-arguments (package-arguments gnu:rust)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'configure 'configure-large-pages
              (lambda _
                (setenv "JEMALLOC_SYS_WITH_LG_PAGE" "16")))))))))

(define-public rust-src
  (@@ (gnu packages rust) rust-src))

(define replace-rust
  (package-input-rewriting
   `((,gnu:rust . ,rust))))
