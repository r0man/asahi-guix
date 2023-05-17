(define-module (asahi guix services udev)
  #:use-module (asahi guix udev)
  #:use-module (gnu services base)
  #:export (%udev-backlight-service
            %udev-kbd-backlight-service))

(define %udev-backlight-service
  (udev-rules-service 'backlight %udev-backlight-rule))

(define %udev-kbd-backlight-service
  (udev-rules-service 'kbd-backlight %udev-kbd-backlight-rule))
