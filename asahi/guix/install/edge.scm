(define-module (asahi guix install edge)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix install base)
  #:use-module (asahi guix packages linux)
  #:use-module (gnu system)
  #:export (asahi-installation-os-edge))

(define asahi-installation-os-edge
  (operating-system
    (inherit asahi-installation-os)
    (kernel asahi-linux-edge)
    (initrd-modules asahi-initrd-modules-edge)))

asahi-installation-os-edge
