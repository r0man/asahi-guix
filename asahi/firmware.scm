(define-module (asahi firmware)
  #:use-module (gnu build file-systems)
  #:use-module (gnu system uuid)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (mount-efi-system-partition))

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
      (uuid (string-trim-right content #\nul)))))

(define (mount-efi-system-partition mount-point)
  (mkdir-p mount-point)
  (while (find-mount-point (mount-points) mount-point)
    (format #t "Unmounting ~a ...\n" mount-point)
    (umount mount-point)
    (format #t "Unmounted ~a\n" mount-point))
  (format #t "Mounting EFI system partition ...\n")
  (let ((boot-path (boot-mount-path))
        (esp-uuid (read-efi-system-partition-uuid efi-system-partition-uuid-path)))
    (when esp-uuid
      (format #t "EFI System partition UUID: ~a\n" (uuid->string esp-uuid))
      (format #t "Mounting ~a to ~a ...\n" (uuid->string esp-uuid) mount-point)
      (let ((partition (find-partition-by-uuid (uuid-bytevector esp-uuid))))
        (if partition
            (mount partition mount-point "ext4")
            (format #t "Can't find partition for UUID: ~a\n" (uuid->string esp-uuid)))))
    (let ((device (find-mount-point (mount-points) mount-point)))
      (if device
          (format #t "Mounted System ESP ~a at ~a\n" (car device) mount-point)
          (format #t "System ESP not mounted.")))))

;; (pretty-print (mount-points))
;; (mount-efi-system-partition "/run/.system-efi")
;; (find-partition-by-uuid "a6543bf9-56ee-4354-aa7b-dc357c7d0d34")
;; (find-partition-by-uuid (uuid "a6543bf9-56ee-4354-aa7b-dc357c7d0d34"))
;; (find-partition-by-uuid (uuid-bytevector (uuid "a6543bf9-56ee-4354-aa7b-dc357c7d0d34")))
;; (uuid->string (uuid "a6543bf9-56ee-4354-aa7b-dc357c7d0d34"))
