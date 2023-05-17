(define-module (asahi guix substitutes)
  #:use-module (gnu services base)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (ice-9 optargs)
  #:export (append-substitutes))

(define %authorized-keys
  (list (local-file "substitutes.asahi-guix.org.pub")))

(define %substitute-urls
  (list "https://substitutes.asahi-guix.org"))

(define* (append-substitutes
          config
          #:key
          (authorized-keys %authorized-keys)
          (substitute-urls %substitute-urls))
  (guix-configuration
   (inherit config)
   (authorized-keys
    (append
     (guix-configuration-authorized-keys config)
     authorized-keys))
   (substitute-urls
    (append
     (guix-configuration-substitute-urls config)
     substitute-urls))))
