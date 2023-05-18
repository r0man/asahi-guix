(define-module (asahi guix build modules)
  #:use-module (ice-9 match)
  #:export (import-asahi-module?))

(define (asahi-module-name? name)
  "Return true if NAME (a list of symbols) denotes a Guix or Asahi module."
  (match name
    (('asahi _ ...) #t)
    (('gnu _ ...) #t)
    (('guix _ ...) #t)
    (_ #f)))

;; Since we don't use deduplication support in 'populate-store', don't
;; import (guix store deduplication) and its dependencies, which
;; includes Guile-Gcrypt.
(define (import-asahi-module? module)
  "Return true if MODULE is not (guix store deduplication)"
  (and (asahi-module-name? module)
       (not (equal? module '(guix store deduplication)))))
