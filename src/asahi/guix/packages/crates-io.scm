(define-module (asahi guix packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public rust-bindgen-cli
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
       (("rust-bindgen" ,rust-bindgen-0.59)
        ("rust-env-logger" ,rust-env-logger-0.9))
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
    (license license:bsd-3)))

(define-public rust-simple-logger-1
  (package
    (name "rust-simple-logger")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "simple_logger" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0pkvkp0v3w9kwqjhx5npb2jbyj9kfbb8y2w92s5cphsxldc05dj5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-atty" ,rust-atty-0.2)
                       ("rust-colored" ,rust-colored-1)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-time" ,rust-time-0.3)
                       ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/borntyping/rust-simple_logger")
    (synopsis
     "A logger that prints all messages with a readable output format")
    (description
     "This package provides a logger that prints all messages with a readable output
format")
    (license license:expat)))

(define-public rust-clap-verbosity-flag-2
  (package
    (name "rust-clap-verbosity-flag")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clap-verbosity-flag" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "06f0myl6chqvyf9dpv3ydblqp8sjrkwwm0nai8vzn33rbl0vpzg5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-clap" ,rust-clap-4)
                       ("rust-log" ,rust-log-0.4))))
    (home-page "https://github.com/clap-rs/clap-verbosity-flag")
    (synopsis "Easily add a `--verbose` flag to CLIs using Clap")
    (description "Easily add a `--verbose` flag to CLIs using Clap")
    (license (list license:expat license:asl2.0))))

(define-public rust-pure-rust-locales-0.7
  (package
    (name "rust-pure-rust-locales")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pure-rust-locales" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0cl46srhxzj0jlvfp73l8l9qw54qwa04zywaxdf73hidwqlsh0pd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/cecton/pure-rust-locales")
    (synopsis
     "Pure Rust locales imported directly from the GNU C Library. `LC_COLLATE` and `LC_CTYPE` are not yet supported.")
    (description
     "Pure Rust locales imported directly from the GNU C Library. `LC_COLLATE` and
`LC_CTYPE` are not yet supported.")
    (license (list license:expat license:asl2.0))))

(define-public rust-android-tzdata-0.1
  (package
    (name "rust-android-tzdata")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "android-tzdata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1w7ynjxrfs97xg3qlcdns4kgfpwcdv824g611fq32cag4cdr96g9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/RumovZ/android-tzdata")
    (synopsis "Parser for the Android-specific tzdata file")
    (description "Parser for the Android-specific tzdata file")
    (license (list license:expat license:asl2.0))))

(define-public rust-chrono-0.4
  (package
    (name "rust-chrono")
    (version "0.4.31")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chrono" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0f6vg67pipm8cziad2yms6a639pssnvysk1m05dd9crymmdnhb3z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-android-tzdata" ,rust-android-tzdata-0.1)
                       ("rust-arbitrary" ,rust-arbitrary-1)
                       ("rust-iana-time-zone" ,rust-iana-time-zone-0.1)
                       ("rust-js-sys" ,rust-js-sys-0.3)
                       ("rust-num-traits" ,rust-num-traits-0.2)
                       ("rust-pure-rust-locales" ,rust-pure-rust-locales-0.7)
                       ("rust-rkyv" ,rust-rkyv-0.7)
                       ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
                       ("rust-serde" ,rust-serde-1)
                       ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
                       ("rust-windows-targets" ,rust-windows-targets-0.48))))
    (home-page "https://github.com/chronotope/chrono")
    (synopsis "Date and time library for Rust")
    (description "Date and time library for Rust")
    (license (list license:expat license:asl2.0))))

(define-public rust-alsa-0.8
  (package
    (name "rust-alsa")
    (version "0.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "alsa" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "02pzlq2q8ml28ikvkvm77bwdqmi22d6ak1qvrc0cr6yjb9adwd6f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-alsa-sys" ,rust-alsa-sys-0.3)
                       ("rust-bitflags" ,rust-bitflags-2)
                       ("rust-libc" ,rust-libc-0.2)
                       ("rust-nix" ,rust-nix-0.26))))
    (home-page "https://github.com/diwic/alsa-rs")
    (synopsis "Thin but safe wrappers for ALSA (Linux sound API)")
    (description "Thin but safe wrappers for ALSA (Linux sound API)")
    (license (list license:asl2.0 license:expat))))

(define-public rust-speakersafetyd-0.1
  (package
    (name "rust-speakersafetyd")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "speakersafetyd" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "18vz3qzalarv688rxmk8xmfbq83jp6bliarchbmmcpfn6ffkngx7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-alsa" ,rust-alsa-0.8)
                       ("rust-chrono" ,rust-chrono-0.4)
                       ("rust-clap" ,rust-clap-4)
                       ("rust-clap-verbosity-flag" ,rust-clap-verbosity-flag-2)
                       ("rust-configparser" ,rust-configparser-2)
                       ("rust-json" ,rust-json-0.12)
                       ("rust-log" ,rust-log-0.4)
                       ("rust-simple-logger" ,rust-simple-logger-1))))
    (inputs
     (list alsa-lib))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/AsahiLinux/speakersafetyd/")
    (synopsis "Speaker protection daemon for embedded Linux systems")
    (description "Speaker protection daemon for embedded Linux systems")
    (license license:expat)))
