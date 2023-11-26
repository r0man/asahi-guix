(define-module (asahi guix packages crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cargo)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
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

(define-public rust-bankstown
  (package
    (name "rust-bankstown")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chadmed/bankstown")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "084p4rf06m4ywiai1lj079p9fa3dkx555vmp5q9dzrmf6fxbsffr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-biquad" ,rust-biquad-0.4)
                       ("rust-lv2" ,rust-lv2-0.6))))
    (home-page "https://github.com/chadmed/bankstown")
    (synopsis "Barebones bass enhancer")
    (description "Halfway-decent three-stage psychoacoustic bass approximation.")
    (license license:expat)))

(define-public rust-biquad-0.4
  (package
    (name "rust-biquad")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "biquad" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0gpc13lag439nmq077wfwz055qbjaxbpk7znvnbddbg3wgsj81c2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libm" ,rust-libm-0.1))))
    (home-page "https://github.com/korken89/biquad-rs")
    (synopsis "A library for digital second order IIR filtrers, also known as
biquads.")
    (description
     "This package provides a library for digital second order IIR filtrers,
also known as biquads.")
    (license (list license:expat license:asl2.0))))

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

(define-public rust-lv2-worker-0.1
  (package
    (name "rust-lv2-worker")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-worker" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "14crsrnjyarra9ipma6lhaj4gpfadvippzr134nkn0z3y30ip4fj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's work offloading library")
    (description "rust-lv2's work offloading library")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-urid-2
  (package
    (name "rust-lv2-urid")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-urid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0s2fcb0nyn54ml6azkbhnnxghy898x1q5vs5qgdznrhy9m20624c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's URID handling library")
    (description "rust-lv2's URID handling library")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-time-0.1
  (package
    (name "rust-lv2-time")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-time" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wznk17vvn5dph6r47vjwmf7g98pb6ij2fdhizdk95sf2qvkf82c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's wrapper of LV2's time types")
    (description "rust-lv2's wrapper of LV2's time types")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-state-2
  (package
    (name "rust-lv2-state")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-state" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0nm0fc7cb4rkmfsvvr4xbac4qf0j7wl2gws3qrcflx057i2lpsb5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's state handling library")
    (description "rust-lv2's state handling library")
    (license (list license:expat license:asl2.0))))

(define-public rust-wmidi-3
  (package
    (name "rust-wmidi")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wmidi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1kxnbs18nmpzm2hfwaaa5h2s77cmk5w53srzxqmrqlkdpdcrjafa"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/RustAudio/wmidi")
    (synopsis "Midi parsing library.")
    (description "Midi parsing library.")
    (license license:expat)))

(define-public rust-lv2-midi-1
  (package
    (name "rust-lv2-midi")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-midi" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0x0glbrfri1glgcrmvc6i1jfv6azhpqvp4ibk5cihsq3s2yfc8xd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1)
                       ("rust-wmidi" ,rust-wmidi-3))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's MIDI processing library")
    (description "rust-lv2's MIDI processing library")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-units-0.1
  (package
    (name "rust-lv2-units")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-units" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0fdamp3hxdr36hqi1j6y01rz1x17if1ibzr7rr4nrabidw74gf82"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's wrapper of LV2's unit types")
    (description "rust-lv2's wrapper of LV2's unit types")
    (license (list license:expat license:asl2.0))))

(define-public rust-urid-derive-0.1
  (package
    (name "rust-urid-derive")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urid-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0i1nf0sgq4ai051h17s9msaavl3jfzdmdlsy8455pr88y0pfx7l1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Procedural macros for urid")
    (description "Procedural macros for urid")
    (license (list license:expat license:asl2.0))))

(define-public rust-urid-0.1
  (package
    (name "rust-urid")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urid" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "195672gs136vczn1r4hkjg5vfa7vdzr26bzv6lwhk0z7cvbvaa38"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-urid-derive" ,rust-urid-derive-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Library for idiomatic URID support")
    (description "Library for idiomatic URID support")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-sys-2
  (package
    (name "rust-lv2-sys")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-sys" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0c4f59mrjyy0z0wf033wp648df0sc6zirrcd6kndqj9nvvkzkl4x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's C header bindings")
    (description "rust-lv2's C header bindings")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-core-derive-2
  (package
    (name "rust-lv2-core-derive")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-core-derive" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "12w3l41jzargrcywz13hbmaazfw4ix2sljl3601h6jfbdrw8zybv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-proc-macro2" ,rust-proc-macro2-1)
                       ("rust-quote" ,rust-quote-1)
                       ("rust-syn" ,rust-syn-1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "Procedural macros for lv2-core")
    (description "Procedural macros for lv2-core")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-core-3
  (package
    (name "rust-lv2-core")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-core" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1pj9l15zwqwj2h83f3xfpwxsj70vvhkw52gyzkljafvrbx1h00fm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-core-derive" ,rust-lv2-core-derive-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's core library")
    (description "rust-lv2's core library")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-atom-2
  (package
    (name "rust-lv2-atom")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2-atom" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "0wd9rgsn8sag8wyhjccmnn82gx4w1yyiav52nyvk579l21xlw6wm"))))
    (build-system cargo-build-system)
    (arguments
     `(#:skip-build? #t
       #:cargo-inputs (("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-lv2-units" ,rust-lv2-units-0.1)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "rust-lv2's Atom handling library")
    (description "rust-lv2's Atom handling library")
    (license (list license:expat license:asl2.0))))

(define-public rust-lv2-0.6
  (package
    (name "rust-lv2")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lv2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1xh4hjfh2w5rhzbk0g9845k25f6fxrv7xqpkr09p0x57b200qc41"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-lv2-atom" ,rust-lv2-atom-2)
                       ("rust-lv2-core" ,rust-lv2-core-3)
                       ("rust-lv2-midi" ,rust-lv2-midi-1)
                       ("rust-lv2-state" ,rust-lv2-state-2)
                       ("rust-lv2-sys" ,rust-lv2-sys-2)
                       ("rust-lv2-time" ,rust-lv2-time-0.1)
                       ("rust-lv2-units" ,rust-lv2-units-0.1)
                       ("rust-lv2-urid" ,rust-lv2-urid-2)
                       ("rust-lv2-worker" ,rust-lv2-worker-0.1)
                       ("rust-urid" ,rust-urid-0.1))))
    (home-page "https://github.com/RustAudio/rust-lv2")
    (synopsis "A safe, fast, and ergonomic framework to create LV2 plugins")
    (description
     "This package provides a safe, fast, and ergonomic framework to create LV2
plugins")
    (license (list license:expat license:asl2.0))))
