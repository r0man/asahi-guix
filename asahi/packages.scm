(define-module (asahi packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public asahi-guix
  (package
    (name "asahi-guix")
    (version "8ccce5a14b4db8ca76c02763133d352c07adaa37")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/r0man/asahi-guix.git")
             (commit version)))
       (sha256
        (base32 "1g6kv4npzys6qs20y5ka90kplf2br7n7h5k44a0y7ybck4r65jwb"))))
    (build-system guile-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-files
           (lambda _
             (delete-file "asahi/installer.scm")
             (delete-file "asahi/initrd.scm"))))))
    (native-inputs
     (list guile-3.0-latest guix))
    (home-page "https://github.com/r0man/asahi-guix")
    (synopsis "Asahi Guix")
    (description "Asahi Guix Guile package")
    (license license:expat)))

(define (make-asahi-linux name config)
  (package
    (inherit
     (customize-linux
      #:name name
      #:source (origin
                 (method url-fetch)
                 (uri (string-append "https://github.com/AsahiLinux/linux/archive/"
                                     "refs/tags/asahi-" version ".tar.gz"))
                 (sha256
                  (base32 "0fnaqih8k7ri6fqaghhh7lyfylf8nj559xqa7qs6fla6isxa0cnf")))))
    (name name)
    (version "6.2-rc2-1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/AsahiLinux/linux/archive/"
                           "refs/tags/asahi-" version ".tar.gz"))
       (sha256
        (base32 "1r8aj2a0ss25pjf49ddfs1hq5vld1zi5pd3f8y2p2dgi2ji4mqag"))))
    (native-inputs
     `(("kconfig" ,config)
       ("zstd" ,zstd)
       ,@(alist-delete "kconfig" (package-native-inputs linux-libre-arm64-generic))))
    (home-page "https://asahilinux.org")
    (synopsis "Linux on Apple Silicon")
    (description "Asahi Linux is a project and community with the goal of porting Linux
to Apple Silicon Macs, starting with the 2020 M1 Mac Mini, MacBook
Air, and MacBook Pro.")))

(define-public asahi-linux
  (make-asahi-linux "asahi-linux" (local-file "kernel.config")))

(define-public asahi-linux-edge
  (make-asahi-linux "asahi-linux-edge" (local-file "kernel.edge.config")))

(define-public asahi-m1n1
  (package
    (name "asahi-m1n1")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/m1n1.git")
             (commit "fb36e302363cbe505da856f127bc0ae5e4a11257")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fm8vhdi75nfwm3n9wy5hrg26wa10h2bgba3250jfv2lg62rsd50"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "RELEASE" "1")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((dir (string-append (assoc-ref outputs "out") "/usr/lib/")))
               (mkdir-p dir)
               (copy-file "build/m1n1.bin" (string-append dir "m1n1.bin")))))
         (delete 'check))))
    (home-page "https://github.com/AsahiLinux/m1n1")
    (synopsis "Experimentation playground for Apple Silicon")
    (description "A bootloader and experimentation playground for Apple Silicon")
    (license license:expat)))

(define-public asahi-fwextract
  (package
    (name "asahi-fwextract")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/asahi-installer.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kj9ycy3f34fzm9bnirlcw9zm2sgipwrqzphdg5k099rbjbc7zmj"))))
    (build-system python-build-system)
    (home-page "https://github.com/AsahiLinux/asahi-installer")
    (synopsis "Asahi Linux firmware extractor")
    (description "The Asahi Linux firmware extraction tool")
    (license license:expat)))

(define-public asahi-scripts
  (package
    (name "asahi-scripts")
    (version "20221220")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/asahi-scripts.git")
             (commit version)))
       (sha256
        (base32 "06a1ixcvnzn9hj1wzfmvvnr9ddgdqqap87b7cf3f92av1a6p6576"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile"
               (("PREFIX=/usr/local") "PREFIX="))
             (setenv "DESTDIR" (assoc-ref outputs "out"))))
         (delete 'check))))
    (home-page "https://github.com/AsahiLinux/asahi-installer")
    (synopsis "Asahi Linux scripts")
    (description "Miscellaneous admin scripts for the Asahi Linux reference distro")
    (license license:expat)))

(define-public lzfse
  (package
    (name "lzfse")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lzfse/lzfse.git")
             (commit (string-append "lzfse-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mfh6y6vpvxsdwmqmfbkqkwvxc0pz2dqqc72c6fk9sbsrxxaghd5"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/lzfse/lzfse")
    (synopsis "LZFSE compression library and command line tool")
    (description "This is a reference C implementation of the LZFSE compressor
introduced in the Compression library with OS X 10.11 and iOS 9. LZFSE
is a Lempel-Ziv style data compression algorithm using Finite State
Entropy coding. It targets similar compression rates at higher
compression and decompression speed compared to deflate using zlib")
    (license license:bsd-3)))

(define-public u-boot-apple-m1
  (let ((base (make-u-boot-package "apple_m1" "aarch64-linux-gnu")))
    (package
      (inherit base)
      (version "2022.10-1")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://github.com/AsahiLinux/u-boot/archive/asahi-v"
               version ".tar.gz"))
         (sha256
          (base32 "02x90h89p1kv3d29mdhq22a88m68w4m1cwb45gj0rr85i2z8mqjq"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (delete 'disable-tools-libcrypto)))))
      (inputs
       `(("openssl" ,libressl)
         ,@(package-inputs base))))))
