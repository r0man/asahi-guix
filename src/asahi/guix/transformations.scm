(define-module (asahi guix transformations)
  #:use-module ((asahi guix packages jemalloc) #:prefix next:)
  #:use-module (asahi guix packages gl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages jemalloc)
  #:use-module (guix packages)
  #:export (replace-asahi
            replace-jemalloc
            replace-libdrm
            replace-mesa))

(define replace-asahi
  (package-input-rewriting
   `((,jemalloc . ,next:jemalloc)
     (,mesa . ,asahi-mesa))))

(define replace-jemalloc
  (package-input-rewriting `((,jemalloc . ,next:jemalloc))))

(define replace-mesa
  (package-input-rewriting `((,mesa . ,asahi-mesa))))
