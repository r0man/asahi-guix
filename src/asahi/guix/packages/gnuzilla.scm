(define-module (asahi guix packages gnuzilla)
  #:use-module ((gnu packages gnuzilla) #:prefix gnu:)
  #:use-module (asahi guix packages jemalloc)
  #:use-module (guix packages))

(define-public asahi-icecat
  (replace-jemalloc
   (package/inherit gnu:icecat
     (name "asahi-icecat"))))
