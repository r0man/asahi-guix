(define-module (asahi guix packages gnuzilla)
  #:use-module ((gnu packages gnuzilla) #:prefix gnu:)
  #:use-module (asahi guix packages rust)
  #:use-module (guix packages))

(define-public asahi-icecat
  (replace-rust
   (package/inherit gnu:icecat
     (name "asahi-icecat"))))
