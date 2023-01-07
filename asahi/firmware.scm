(define-module (asahi firmware)
  #:use-module (gnu build file-systems)
  #:use-module (gnu system uuid)
  #:use-module (guix build syscalls)
  #:use-module (guix build utils)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:export (mount-efi-system-partition))

(define %default-efi-device
  "/dev/nvme0n1p7")

(define %efi-system-partition-uuid-path
  "/proc/device-tree/chosen/asahi,efi-system-partition")

(define (boot-mount-path)
  "Return the current boot mount path."
  (cond ((file-exists? "/boot/efi/m1n1")
         "/boot/efi")
        ((file-exists? "/boot/m1n1")
         "/boot")))

(define (disk-devices)
  (map (lambda (device) (format #f "/dev/~a" device)) (disk-partitions)))

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

(define (efi-system-partition-device path)
  %default-efi-device)

(define (unmount-efi-system-partition mount-point)
  (when (directory-exists? mount-point)
    (while (find-mount-point (mount-points) mount-point)
      (format #t "Unmounting ~a ...\n" mount-point)
      (umount mount-point)
      (format #t "Unmounted ~a\n" mount-point))))

(define (mount-efi-system-partition mount-point)
  (unmount-efi-system-partition mount-point)
  (format #t "Mounting EFI system partition ...\n")
  (mkdir-p mount-point)
  (let ((boot-path (boot-mount-path))
        (esp-uuid (read-efi-system-partition-uuid %efi-system-partition-uuid-path)))
    (when esp-uuid
      (format #t "EFI System partition UUID: ~a\n" (uuid->string esp-uuid))
      (format #t "Mounting ~a to ~a ...\n" (uuid->string esp-uuid) mount-point)
      (mount %default-efi-device mount-point "vfat"))
    (let ((device (find-mount-point (mount-points) mount-point)))
      (if device
          (format #t "Mounted System ESP ~a at ~a\n" (car device) mount-point)
          (format #t "System ESP not mounted.")))))

;; (define (extract-firmware mount-point)
;;   (let ((esp-mount-dir "/tmp/.fwsetup/esp")
;;         (extracted-firmware-dir "/tmp/.fwsetup/esp"))
;;     (mkdir-p "/tmp/.fwsetup/esp")
;;     (mkdir-p "/tmp/.fwsetup/extracted")))

;; (pretty-print (mount-points))
;; (mount-efi-system-partition "/run/.system-efi")
;; (find-partition-by-uuid "a6543bf9-56ee-4354-aa7b-dc357c7d0d34")
;; (find-partition-by-uuid (uuid "a6543bf9-56ee-4354-aa7b-dc357c7d0d34"))
;; (find-partition-by-uuid (uuid-bytevector (uuid "a6543bf9-56ee-4354-aa7b-dc357c7d0d34")))
;; (uuid->string (uuid "a6543bf9-56ee-4354-aa7b-dc357c7d0d34"))
