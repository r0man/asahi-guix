(define-module (asahi guix system edge)
  #:use-module (asahi guix initrd)
  #:use-module (asahi guix packages)
  #:use-module (asahi guix system base)
  #:use-module (asahi guix transformations)
  #:use-module (gnu system)
  #:export (asahi-operating-system-edge))

(define asahi-operating-system-edge
  (operating-system
    (inherit asahi-operating-system)
    (kernel (replace-jemalloc asahi-linux-edge))
    (initrd-modules asahi-initrd-modules-edge)))

asahi-operating-system-edge
