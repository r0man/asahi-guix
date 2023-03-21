(define-module (asahi guix packages xdisorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

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
