(define-module (asahi guix udev)
  #:use-module (gnu services base)
  #:export (%udev-backlight-rule
            %udev-kbd-backlight-rule))

(define %udev-backlight-rule
  (udev-rule
   "90-backlight.rules"
   (string-append
    "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
    "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/backlight/%k/brightness\""
    "\n"
    "ACTION==\"add\", SUBSYSTEM==\"backlight\", "
    "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/backlight/%k/brightness\"")))

(define %udev-kbd-backlight-rule
  (udev-rule
   "90-kbd-backlight.rules"
   (string-append
    "ACTION==\"add\", SUBSYSTEM==\"leds\", "
    "RUN+=\"/run/current-system/profile/bin/chgrp video /sys/class/leds/kbd_backlight/brightness\""
    "\n"
    "ACTION==\"add\", SUBSYSTEM==\"leds\", "
    "RUN+=\"/run/current-system/profile/bin/chmod g+w /sys/class/leds/kbd_backlight/brightness\"")))
