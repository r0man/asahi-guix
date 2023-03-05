(define-module (asahi guix packages)
  #:use-module ((asahi guix packages jemalloc) #:prefix jemalloc-next:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix transformations)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define replace-jemalloc
  (package-input-rewriting `((,jemalloc . ,jemalloc-next:jemalloc))))

(define-public asahi-audio
  (package
    (name "asahi-audio")
    (version "5f9067d0fba89acb6c6d68819edad30fe28b1dfe")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chadmed/asahi-audio")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0048qx4afvm1qfayzzfia7iqbj17pkz5xspya74xlc5ji3k3vfij"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("conf" "etc/pipewire/pipewire.conf.d")
         ("firs" "usr/share/pipewire/devices/apple"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "conf/j314.conf"
                 (("/usr/share/pipewire/devices/apple")
                  (string-append out "/usr/share/pipewire/devices/apple")))
               (substitute* "conf/j316.conf"
                 (("/usr/share/pipewire/devices/apple")
                  (string-append out "/usr/share/pipewire/devices/apple")))))))))
    (home-page "https://github.com/chadmed/asahi-audio")
    (synopsis "Linux audio configuration for Apple Silicon Macs")
    (description "Linux userspace audio configuration for Apple Silicon Macs.")
    (license license:expat)))

(define-public alsa-ucm-conf-asahi
  (package
    (name "alsa-ucm-conf-asahi")
    (version "1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/AsahiLinux/alsa-ucm-conf-asahi/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "0rw16mk0msj518aq8prjhm0m9fm3x26zrxz7wnc2nxnw52vzbdaa"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("ucm2" "share/alsa/ucm2"))))
    (home-page "https://github.com/AsahiLinux/alsa-ucm-conf-asahi")
    (synopsis "The Advanced Linux Sound Architecture Use Case Manager")
    (description
     "This package contains Advanced Linux Sound Architecture Use Case Manager
configuration of audio input/output names and routing for specific audio
hardware.")
    (license license:bsd-3)))

(define-public asahi-firmware
  (package
    (name "asahi-firmware")
    (version "1.0.0")
    (source (cond
             ((getenv "ASAHI_GUIX_FIRMWARE_SOURCE")
              (local-file (getenv "ASAHI_GUIX_FIRMWARE_SOURCE")))
             ((file-exists? "/boot/efi/vendorfw/firmware.cpio")
              (local-file "/boot/efi/vendorfw/firmware.cpio"))
             ((file-exists? "/run/.system-efi/vendorfw/firmware.cpio")
              (local-file "/run/.system-efi/vendorfw/firmware.cpio"))
             (else (display "WARNING: Apple Silicon firmware was not found !!!\n\n")
                   (display "Please set either the ASAHI_GUIX_FIRMWARE_SOURCE environment variable
to a file named firmware.cpio, or make it in one of the following
locations available:\n\n")
                   (display "- /boot/efi/vendorfw/firmware.cpio\n")
                   (display "- /run/.system-efi/vendorfw/firmware.cpio\n\n")
                   (local-file "firmware.cpio"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("vendorfw" "lib/firmware"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-firmware
           (lambda _
             (invoke "cpio" "-idv" "-F" "firmware.cpio"))))))
    (native-inputs (list cpio))
    (home-page "https://github.com/r0man/asahi-guix")
    (synopsis "Asahi Guix firmware for Apple Silicon")
    (description "The Asahi Guix firmware package uses the Apple Silicon firmware from
the local machine as source.  The Apple Silicon firmware is
propriatary and can not be packaged.")
    (license license:expat)))

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
    (license license:bsd-3)))

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

(define (make-asahi-linux name config)
  (let* ((version "6.2-rc3-6")
         (base (customize-linux
                #:linux linux-libre-arm64-generic
                #:name name
                #:source (origin
                           (method url-fetch)
                           (uri (string-append "https://github.com/AsahiLinux/linux/archive/"
                                               "asahi-" version ".tar.gz"))
                           (sha256
                            (base32 "0bk4grzcizk48hhalyyaa4alk5069z102vx5ddw12jfqzsrdfccn"))))))
    (package
      (inherit base)
      (version version)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (add-before 'configure 'configure-rust
                (lambda* (#:key inputs #:allow-other-keys)
                  (setenv "LIBCLANG_PATH"
                          (string-append (assoc-ref inputs "clang") "/lib"))
                  (setenv "RUST_LIB_SRC"
                          (string-append (assoc-ref inputs "rust-src")
                                         "/lib/rustlib/src/rust/library"))))
              (replace 'configure
                (lambda* (#:key inputs #:allow-other-keys)
                  (copy-file #$config ".config")
                  (chmod ".config" #o644)))))))
      (native-inputs
       `(("clang" ,clang)
         ("llvm" ,llvm)
         ("python" ,python)
         ("rust" ,(replace-jemalloc (@@ (gnu packages rust) rust-1.62)))
         ("rust-bindgen-cli" ,(replace-jemalloc rust-bindgen-cli))
         ("rust-src" ,rust-src-1.62)
         ("zstd" ,zstd)
         ,@(package-native-inputs base)))
      (home-page "https://asahilinux.org")
      (synopsis "Linux on Apple Silicon")
      (description "Asahi Linux is a project and community with the goal of porting Linux
to Apple Silicon Macs, starting with the 2020 M1 Mac Mini, MacBook
Air, and MacBook Pro."))))

(define-public asahi-linux
  (make-asahi-linux "asahi-linux" (local-file "kernel.config")))

(define-public asahi-linux-edge
  (make-asahi-linux "asahi-linux-edge" (local-file "kernel.edge.config")))

(define-public asahi-bootlogo
  (package
    (name "asahi-bootlogo")
    (version "0.0.1")
    (source (local-file "bootlogo.svg"))
    (build-system copy-build-system)
    (home-page "https://www.gnu.org/graphics/heckert_gnu.html")
    (synopsis "A Bold GNU Head SVG")
    (description "A bolder and simpler version of the famous GNU logo, which looks great
at low resolutions and works well at high resolutions for printing.")
    (license license:expat)))

(define-public asahi-m1n1
  (package
    (name "asahi-m1n1")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/AsahiLinux/m1n1/archive/v"
                           version ".tar.gz"))
       (sha256
        (base32 "1pymb7ip77z8md1pxqm3micq2yns1v6b97mayaa2q1s8sinv00jg"))))
    (build-system gnu-build-system)
    (supported-systems (list "aarch64-linux"))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'bootlogo
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bootlogo (assoc-ref inputs "asahi-bootlogo"))
                    (bootlogo-input (string-append bootlogo "/bootlogo.svg"))
                    (bootlogo-path (string-append out "/bootlogo.png")))
               (when (file-exists? bootlogo-input)
                 (chdir "data")
                 (delete-file "bootlogo_128.png")
                 (invoke "convert"
                         "-background" "none"
                         "-resize" "128x128"
                         bootlogo-input
                         "bootlogo_128.png")
                 (delete-file "bootlogo_256.png")
                 (invoke "convert"
                         "-background" "none"
                         "-resize" "256x256"
                         bootlogo-input
                         "bootlogo_256.png")
                 (invoke "./makelogo.sh")
                 (chdir "..")))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "RELEASE" "1")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((dir (string-append (assoc-ref outputs "out") "/libexec/")))
               (mkdir-p dir)
               (copy-file "build/m1n1.bin" (string-append dir "m1n1.bin")))))
         ;; There are no tests
         (delete 'check))))
    (home-page "https://github.com/AsahiLinux/m1n1")
    (native-inputs (list asahi-bootlogo imagemagick))
    (synopsis "Boot loader and experimentation playground for Apple Silicon")
    (description "m1n1 is the bootloader developed by the Asahi Linux project to bridge
the Apple (XNU) boot ecosystem to the Linux boot ecosystem.")
    (license license:expat)))

(define-public asahi-fwextract
  (package
    (name "asahi-fwextract")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AsahiLinux/asahi-installer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kj9ycy3f34fzm9bnirlcw9zm2sgipwrqzphdg5k099rbjbc7zmj"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-vendor
           (lambda* (#:key outputs #:allow-other-keys)
             (delete-file-recursively "vendor")))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/asahi-fwextract")
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "lzfse") "/lib"))))))))))
    (inputs (list lzfse))
    (home-page "https://github.com/AsahiLinux/asahi-installer")
    (synopsis "Asahi Linux firmware extractor")
    (description "The Asahi Linux firmware extractor transform the firmware archive
provided by the Asahi Linux installer into a manifest and CPIO and TAR
archives that are compatible with the Linux kernel.")
    (license license:expat)))

(define-public libdrm-2-4-114
  (package
    (inherit libdrm)
    (version "2.4.114")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "09nhk3jx3qzggl5vyii3yh4zm0npjqsbxhzvxrg2xla77a2cyj9h"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libdrm)
       ((#:configure-flags flags)
        `(list "-Dexynos=enabled"
               "-Domap=enabled"
               "-Detnaviv=enabled"
               "-Dtegra=enabled"
               "-Dfreedreno=enabled"
               "-Dfreedreno-kgsl=true"))))
    (inputs
     `(("wayland-protocols" ,wayland-protocols-next)
       ,@(package-inputs libdrm)))))

(define-public mesa-asahi-edge
  (package
    (inherit mesa)
    (name "mesa-asahi-edge")
    (version "20221229")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.freedesktop.org/asahi/mesa/-/archive/"
                           "asahi-" version "/mesa-asahi-" version ".tar.gz"))
       (sha256
        (base32 "1gg0msrx2d2mgif4jqljns8nqf29nazqpxcxmjaa50yf50n6n05p"))))
    (arguments
     (substitute-keyword-arguments (package-arguments mesa)
       ((#:configure-flags flags)
        `(list "-Db_ndebug=true"
               "-Db_lto=false"
               "-Ddri3=enabled"
               "-Degl=enabled"
               "-Dgallium-drivers=swrast,virgl,kmsro,asahi"
               "-Dgallium-extra-hud=true"
               "-Dgallium-opencl=disabled"
               "-Dgallium-rusticl=false"
               "-Dgallium-va=disabled"
               "-Dgallium-vdpau=disabled"
               "-Dgallium-xa=disabled"
               "-Dgbm=enabled"
               "-Dgles1=disabled"
               "-Dgles2=enabled"
               "-Dglx=dri"
               "-Dlibunwind=disabled"
               "-Dllvm=enabled"
               "-Dlmsensors=enabled"
               "-Dmicrosoft-clc=disabled"
               "-Dosmesa=true"
               "-Dplatforms=x11,wayland"
               "-Dshared-glapi=enabled"
               "-Dvalgrind=enabled"
               "-Dvulkan-drivers=swrast"
               "-Dvulkan-layers="))))
    (inputs
     `(("libdrm" ,libdrm-2-4-114)
       ("libglvnd" ,libglvnd)
       ("llvm" ,llvm-15)
       ("lm-sensors" ,lm-sensors "lib")
       ("openssl" ,libressl)
       ("valgrind" ,valgrind)
       ("wayland-protocols" ,wayland-protocols-next)
       ,@(package-inputs mesa)))))

(define-public mesa-asahi-edge-headers
  (package/inherit mesa-headers
    (name "mesa-asahi-edge-headers")
    (version "20221229")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.freedesktop.org/asahi/mesa/-/archive/"
                           "asahi-" version "/mesa-asahi-" version ".tar.gz"))
       (sha256
        (base32 "1gg0msrx2d2mgif4jqljns8nqf29nazqpxcxmjaa50yf50n6n05p"))))))

(define-public asahi-mesa-utils
  (package/inherit mesa-utils
    (name "asahi-mesa-utils")
    (version "8.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.mesa3d.org/demos/" version
                           "/mesa-demos-" version ".tar.bz2"))
       (sha256 (base32 "1hdaf7pnh5h4f16pzrxqw3g5s37r5dkimsy46pv316phh05dz8nf"))))
    (build-system meson-build-system)
    (inputs
     (list mesa-asahi-edge freeglut glew))
    (native-inputs
     (list mesa-asahi-edge-headers pkg-config))))

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
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "Makefile"
                 (("PREFIX=/usr/local") "PREFIX="))
               (substitute* "asahi-fwextract"
                 (("/usr/share/asahi-scripts/functions.sh")
                  (string-append out "/share/asahi-scripts/functions.sh"))
                 (("python3")
                  (string-append (assoc-ref inputs "python") "/bin/python3")))
               (substitute* "update-grub"
                 (("/usr/share/asahi-scripts/functions.sh")
                  (string-append out "/share/asahi-scripts/functions.sh")))
               (substitute* "update-m1n1"
                 (("/usr/share/asahi-scripts/functions.sh")
                  (string-append out "/share/asahi-scripts/functions.sh"))
                 (("/usr/lib/asahi-boot/")
                  (string-append (assoc-ref inputs "asahi-m1n1") "/libexec/"))
                 (("\\$SOURCE/u-boot-nodtb.bin")
                  (string-append (assoc-ref inputs "u-boot-apple-m1") "/libexec/u-boot-nodtb.bin"))
                 (("/lib/modules/\\*-ARCH/dtbs/\\*.dtb")
                  (string-append (assoc-ref inputs "linux") "/lib/dtbs/apple/*.dtb"))))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (setenv "DESTDIR" (assoc-ref outputs "out"))))
         (delete 'check)
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/asahi-fwextract")
                 `("GUIX_PYTHONPATH" ":" prefix
                   (,(getenv "GUIX_PYTHONPATH")))
                 `("LD_LIBRARY_PATH" ":" prefix
                   (,(string-append (assoc-ref inputs "lzfse") "/lib"))))))))))
    (inputs `(("asahi-fwextract" ,asahi-fwextract)
              ("asahi-m1n1" ,asahi-m1n1)
              ("lzfse" ,lzfse)
              ("python" ,python)
              ("u-boot-apple-m1" ,u-boot-apple-m1)
              ("linux" ,asahi-linux)))
    (home-page "https://github.com/AsahiLinux/asahi-scripts")
    (synopsis "Asahi Linux scripts")
    (description "Miscellaneous admin scripts for the Asahi Linux reference distro")
    (license license:expat)))

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
      (native-inputs
       `(("openssl" ,libressl)
         ,@(package-native-inputs base))))))
