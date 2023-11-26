(define-module (asahi guix packages audio)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system copy)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages))

(define-public alsa-ucm-conf-asahi
  (package
    (name "alsa-ucm-conf-asahi")
    (version "4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/AsahiLinux/alsa-ucm-conf-asahi/archive/v"
                    version ".tar.gz"))
              (sha256
               (base32
                "1hrhf3sry2jb5maj1s9mm0bn7c4pfnmlv93aq2dybcsksp591fir"))))
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
