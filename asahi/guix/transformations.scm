(define-module (asahi guix transformations)
  #:use-module (guix transformations)
  #:export (replace-jemalloc))

(define replace-jemalloc
  (options->transformation
   '((with-input . "jemalloc=jemalloc@5.3.0"))))
