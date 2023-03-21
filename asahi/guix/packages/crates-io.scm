(define-module (asahi guix packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (asahi guix packages jemalloc)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages llvm)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix packages))

(define-public rust-bindgen-cli
  (replace-jemalloc
   (package
     (name "rust-bindgen-cli")
     (version "0.59.2")
     (source
      (origin
        (method url-fetch)
        (uri (crate-uri "bindgen-cli" version))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32 "1f4fpycxmbrqk8r2x9brhfgjh86mzc6bngn4a9631x78b2jaklib"))))
     (build-system cargo-build-system)
     (arguments
      `(#:cargo-inputs
        (("rust-bindgen" ,rust-bindgen-0.59))
        #:phases
        (modify-phases %standard-phases
          (add-before 'check 'disable-commandline-multiple-headers-test
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "src/main.rs"
                (("fn commandline_multiple_headers")
                 "#[ignore]\n    fn commandline_multiple_headers")))))))
     (inputs (list clang))
     (home-page "https://rust-lang.github.io/rust-bindgen/")
     (synopsis "Generate Rust FFI bindings to C and C++ libraries")
     (description "This package is the CLI to rust-bindgen and can be used to
automatically generate Rust FFI bindings to C and C++ libraries.")
     (license license:bsd-3))))
