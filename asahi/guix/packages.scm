(define-module (asahi guix packages)
  #:use-module ((asahi guix packages jemalloc) #:prefix jemalloc-next:)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
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
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
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
             (else #f)))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (let ((out (assoc-ref %outputs "out"))
                (cpio (search-input-file %build-inputs "/bin/cpio"))
                (source (assoc-ref %build-inputs "source")))
            (if (and source (file-exists? source))
                (let ((target (string-append out "/lib/firmware")))
                  (invoke cpio "-idv" "-F" source)
                  (mkdir-p target)
                  (copy-recursively "vendorfw" target))
                (begin
                  (display "WARNING: Apple Silicon firmware was not found !!!\n\n")
                  (display "Please set either the ASAHI_GUIX_FIRMWARE_SOURCE environment variable
to a file named firmware.cpio, or make it in one of the following
locations available:\n\n")
                  (display "- /boot/efi/vendorfw/firmware.cpio\n")
                  (display "- /run/.system-efi/vendorfw/firmware.cpio\n\n")
                  (mkdir-p out)))))))
    (native-inputs (list cpio))
    (home-page "https://github.com/r0man/asahi-guix")
    (synopsis "Asahi Guix firmware for Apple Silicon")
    (description "The Asahi Guix firmware package uses the Apple Silicon firmware from
the local machine as source.  The Apple Silicon firmware is
propriatary and can not be packaged.")
    (license license:expat)))

(define rust-jemalloc-fix
  (replace-jemalloc (@@ (gnu packages rust) rust-1.62)))

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

(define rust-bindgen-cli-jemalloc-fix
  (replace-jemalloc rust-bindgen-cli))

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
              (add-before 'configure 'configure-libclang
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((clang (assoc-ref inputs "clang")))
                    (when clang
                      (setenv "LIBCLANG_PATH" (string-append clang "/lib"))))))
              (add-before 'configure 'configure-rust-src
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((rust-src (assoc-ref inputs "rust-src")))
                    (when rust-src
                      (setenv "RUST_LIB_SRC"
                              (string-append rust-src
                                             "/lib/rustlib/src/rust/library"))))))
              (replace 'configure
                (lambda* (#:key inputs #:allow-other-keys)
                  (copy-file #$config ".config")
                  (chmod ".config" #o644)))))))
      (home-page "https://asahilinux.org")
      (synopsis "Linux on Apple Silicon")
      (description "Asahi Linux is a project and community with the goal of porting Linux
to Apple Silicon Macs, starting with the 2020 M1 Mac Mini, MacBook
Air, and MacBook Pro."))))

(define-public asahi-linux
  (make-asahi-linux "asahi-linux" (local-file "kernel.config")))

(define-public asahi-linux-edge
  (let ((base (make-asahi-linux "asahi-linux-edge" (local-file "kernel.edge.config"))))
    (package/inherit base
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend clang
                  llvm
                  python
                  rust-jemalloc-fix
                  rust-bindgen-cli-jemalloc-fix
                  rust-src-1.62
                  zstd))))))

(define-public asahi-m1n1
  (let ((commit "46f2811351806aafb3d56e02c107f95ac2ea85e3"))
    (package
      (name "asahi-m1n1")
      (version (git-version "1.2.4" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AsahiLinux/m1n1")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "14chrqbs57v6i5vmf643svbi3s7h4fxrxly0bby7brf3w114nmpk"))))
      (build-system gnu-build-system)
      (supported-systems (list "aarch64-linux"))
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (replace 'configure
              (lambda _
                (setenv "RELEASE" "1")))
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((dir (string-append (assoc-ref outputs "out") "/libexec/")))
                  (mkdir-p dir)
                  (copy-file "build/m1n1.bin" (string-append dir "m1n1.bin")))))
            ;; There are no tests
            (delete 'check))))
      (home-page "https://github.com/AsahiLinux/m1n1")
      (synopsis "Boot loader and experimentation playground for Apple Silicon")
      (description "m1n1 is the bootloader developed by the Asahi Linux project to bridge
the Apple (XNU) boot ecosystem to the Linux boot ecosystem.")
      (license license:expat))))

(define-public asahi-fwextract
  (let ((commit "0ac64c9ce1c460f4576162a82d239d7e8688a79e"))
    (package
      (name "asahi-fwextract")
      (version (git-version "0.5.3" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AsahiLinux/asahi-installer")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1kj9ycy3f34fzm9bnirlcw9zm2sgipwrqzphdg5k099rbjbc7zmj"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (delete-file-recursively "vendor")
             (with-output-to-file "entry_points.txt"
               (lambda ()
                 (format #t "[console_scripts]\n")
                 (format #t "asahi-fwextract = asahi_firmware.update:main")))))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'create-entrypoints 'wrap-program
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
      (license license:expat))))

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

(define-public asahi-mesa
  (let ((commit "0a12b60a6b4363315ca3789e7e289240704a26da"))
    (package/inherit mesa
      (name "asahi-mesa")
      (version (git-version "20221229" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/asahi/mesa")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0yjn55qy4890gi1s3dhzdhqqxrms4fzcibqr84a3vcc53ggiywmb"))))
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
       (modify-inputs (package-inputs mesa)
         (prepend `(,lm-sensors "lib") libglvnd libressl valgrind)
         (replace "llvm" llvm-15)
         (replace "wayland-protocols" wayland-protocols-next)))
      (propagated-inputs
       (modify-inputs (package-propagated-inputs mesa)
         (replace "libdrm" libdrm-2-4-114))))))

(define-public asahi-mesa-headers
  (package/inherit mesa-headers
    (name "asahi-mesa-headers")
    (version (package-version asahi-mesa))
    (source (package-source asahi-mesa))))

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
     (modify-inputs (package-inputs mesa-utils)
       (replace "mesa" asahi-mesa)))))

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
  (let ((base (make-u-boot-package "apple_m1" "aarch64-linux-gnu"))
        (commit "54409548c3aa8cf4820f1bda69a26bb603a0a5a4"))
    (package/inherit base
      (version (git-version "2022.10-1" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/AsahiLinux/u-boot")
               (commit commit)))
         (file-name (git-file-name (package-name base) version))
         (sha256
          (base32 "1m1w6ajzsfpb59abncz3sa9b1waqjsnh2vm7js2n22xiw4km7nzx"))))
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (delete 'disable-tools-libcrypto)))))
      (native-inputs
       (modify-inputs (package-native-inputs base)
         (prepend libressl))))))
