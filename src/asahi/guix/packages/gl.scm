(define-module (asahi guix packages gl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public asahi-mesa
  (let ((commit "4acd685189c341c66a60d947384f2c9ef5b84b9d"))
    (package/inherit mesa
      (name "asahi-mesa")
      (version (git-version "20230904" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/asahi/mesa.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0219rc1nbqsz1v0psz588yh64ap3ga4k02hyf6vzkx8lbz6xf5w4"))))
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
                 "-Dvulkan-layers="))
         ((#:phases phases '%standard-phases)
          #~(modify-phases #$phases
              (delete 'set-layer-path-in-manifests)))))
      (inputs
       (modify-inputs (package-inputs mesa)
         (prepend `(,lm-sensors "lib") libglvnd libressl valgrind)
         (replace "llvm" llvm-15))))))

(define-public asahi-mesa-headers
  (package/inherit mesa-headers
    (name "asahi-mesa-headers")
    (version (package-version asahi-mesa))
    (source (package-source asahi-mesa))))

(define-public asahi-mesa-utils
  ;; TODO: glxinfo and friends are not compiled
  (package/inherit mesa-utils
    (name "asahi-mesa-utils")
    (version "9.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.mesa3d.org/demos"
                           "/mesa-demos-" version ".tar.xz"))
       (sha256 (base32 "0ss9xpqykwfzkhr55nbfml61dsxz4dgpl9fxxgvil1bvdb9a6iih"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (invoke "ninja" "install"))))))
    (inputs
     (modify-inputs (package-inputs mesa-utils)
       (replace "mesa" asahi-mesa)))))
