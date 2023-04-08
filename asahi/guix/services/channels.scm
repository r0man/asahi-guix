(define-module (asahi guix services channels)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (%asahi-channels-service))

(define %asahi-channels-service
  (simple-service
   'asahi-channels-service etc-service-type
   (list `("guix/channels.scm" ,(local-file "../channels.scm")))))
