(define-module (asahi firmware)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1))

(define efi-system-partition-uuid-path
  "/proc/device-tree/chosen/asahi,efi-system-partition")

(define (boot-mount-path)
  "Return the current boot mount path."
  (cond ((file-exists? "/boot/efi/m1n1")
         "/boot/efi")
        ((file-exists? "/boot/m1n1")
         "/boot")))

(define (mount-points)
  "Return the mount points from /proc/mounts."
  (let ((content (call-with-input-file "/proc/mounts" get-string-all)))
    (map (lambda (s)
           (string-split s #\space))
         (remove! (lambda (s) (string= "" s))
                  (string-split content #\newline)))))

(define (find-mount-point mount-points name)
  "Find the mount point by NAME in MOUNT-POINTS."
  (find (lambda (x) (string= name (cadr x))) mount-points))

(define (read-efi-system-partition-uuid path)
  (when (file-exists? path)
    (let ((content (call-with-input-file path get-string-all)))
      (string-trim-right content #\nul))))

(define (mount-efi-system-partition mount-point)
  (format #t "Mounting EFI system partition ...\n")
  (let ((boot-path (boot-mount-path))
        (esp-uuid (read-efi-system-partition-uuid efi-system-partition-uuid-path))
        (device (find-mount-point (mount-points) mount-point)))
    (format #t "Boot path: ~a\n" boot-path)
    (format #t "EFI System partition UUID: ~a\n" esp-uuid)
    (if device
        (format #t "Mounted System ESP ~a at ~a\n" (car device) mount-point)
        (format #t "System ESP not mounted."))))

;; (pretty-print (mount-points))
;; (mount-efi-system-partition "/run/.system-efi")
