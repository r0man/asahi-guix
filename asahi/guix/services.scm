(define-module (asahi guix services)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (%channels-service))

(define %channels-service
  (simple-service 'channel-file etc-service-type
                  (list `("channels.scm" ,(local-file "channels.scm")))))
