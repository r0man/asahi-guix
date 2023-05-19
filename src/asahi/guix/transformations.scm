(define-module (asahi guix transformations)
  #:use-module ((gnu packages gl) #:prefix gl:)
  #:use-module ((gnu packages rust) #:prefix rust:)
  #:use-module (asahi guix packages gl)
  #:use-module (asahi guix packages rust)
  #:use-module (guix packages)
  #:export (replace-asahi
            replace-mesa))

(define replace-asahi
  (package-input-rewriting
   `((,gl:mesa . ,asahi-mesa)
     (,rust:rust . ,rust))))

(define replace-mesa
  (package-input-rewriting `((,gl:mesa . ,asahi-mesa))))
