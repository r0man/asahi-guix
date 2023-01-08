(define-module (asahi initrd)
  #:use-module (asahi firmware)
  #:use-module (asahi packages)
  #:use-module (gnu build activation)
  #:use-module (gnu build linux-modules)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system mapped-devices)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (ice-9 exceptions)
  #:use-module (ice-9 pretty-print)
  #:export (asahi-initrd))

(define flat-linux-module-directory
  (@@ (gnu system linux-initrd) flat-linux-module-directory))

(define* (asahi-initrd file-systems
                       #:key
                       (linux linux-libre)
                       (linux-modules '())
                       (pre-mount #t)
                       (mapped-devices '())
                       (keyboard-layout #f)
                       (helper-packages (list asahi-guix asahi-firmware))
                       qemu-networking?
                       volatile-root?
                       (on-error 'debug))

  (define device-mapping-commands
    ;; List of gexps to open the mapped devices.
    (map (lambda (md)
           (let* ((source  (mapped-device-source md))
                  (targets (mapped-device-targets md))
                  (type    (mapped-device-type md))
                  (open    (mapped-device-kind-open type)))
             (open source targets)))
         mapped-devices))

  (define file-system-scan-commands
    ;; File systems like btrfs need help to assemble multi-device file systems
    ;; but do not use manually-specified <mapped-devices>.
    (let ((file-system-types (map file-system-type file-systems)))
      (if (member "btrfs" file-system-types)
          ;; Ignore errors: if the system manages to boot anyway, the better.
          #~((system* (string-append #$btrfs-progs/static "/bin/btrfs")
                      "device" "scan"))
          #~())))

  (define kodir
    (flat-linux-module-directory linux linux-modules))

  (define firmware-directory
    (file-append asahi-firmware "/lib/firmware"))

  ;; (format #t "LINUX: ~a\n" linux)
  ;; (format #t "HELPER: ~a\n" helper-packages)
  ;; (format #t "FILE SYSTEMS: ~a\n" file-systems)
  ;; (format #t "MAPPED DEVICES: ~a\n" mapped-devices)
  ;; (format #t "DEVICE MAPPING COMMANDS: ~a\n" device-mapping-commands)

  (expression->initrd
   (with-extensions (list asahi-guix asahi-firmware)
     (with-imported-modules (source-module-closure
                             '((asahi firmware)
                               (gnu build activation)
                               (gnu build file-systems)
                               (gnu build linux-boot)
                               (gnu build linux-modules)
                               (gnu system file-systems)
                               (guix build bournish)
                               (guix build utils)
                               (ice-9 exceptions)))
       #~(begin
           (use-modules (asahi firmware)
                        (gnu build activation)
                        (gnu build linux-boot)
                        (gnu system file-systems)
                        ((guix build utils) #:hide (delete))
                        (guix build bournish)   ;add the 'bournish' meta-command
                        (srfi srfi-1)           ;for lvm-device-mapping
                        (srfi srfi-26)
                        (gnu build linux-modules)
                        (ice-9 exceptions)
                        (ice-9 pretty-print)
                        ;; FIXME: The following modules are for
                        ;; LUKS-DEVICE-MAPPING.  We should instead propagate
                        ;; this info via gexps.
                        ((gnu build file-systems)
                         #:select (find-partition-by-luks-uuid))
                        (rnrs bytevectors))

           (with-output-to-port (%make-void-port "w")
             (lambda ()
               (set-path-environment-variable "PATH" '("bin" "sbin")
                                              '#$helper-packages)))

           (parameterize ((current-warning-port (%make-void-port "w")))
             (boot-system #:mounts
                          (map spec->file-system
                               '#$(map file-system->spec file-systems))
                          #:pre-mount (lambda ()
                                        ;; (guard (ex (else (format #t ":: Asahi Guix: Failed to load firmware.\n")
                                        ;;                  (pretty-print ex)
                                        ;;                  #t))
                                        ;;   (let ((firmware-directory #+(file-append asahi-firmware "/lib/firmware/vendor")))
                                        ;;     (format #t ":: Asahi Guix: Activating firmware in ~a...\n" firmware-directory)
                                        ;;     (activate-firmware firmware-directory)
                                        ;;     (format #t ":: Asahi Guix: Firmware activated.\n")))

                                        ;; (guard (ex (else (format #t ":: Asahi Guix: Failed to load kernel modules.\n")
                                        ;;                  (pretty-print ex)
                                        ;;                  #t))
                                        ;;   (format #t ":: Asahi Guix: Loading kernel modules from ~a...\n" '#$kodir)
                                        ;;   (load-linux-modules-from-directory '#$linux-modules '#$kodir)
                                        ;;   (format #t ":: Asahi Guix: Kernel modules loaded.\n"))

                                        ;; (display ":: Asahi Guix: Mounting EFI system partition ...\n")
                                        ;; (mount-efi-system-partition "/run/.system-efi")
                                        (and #$pre-mount
                                             #$@device-mapping-commands
                                             #$@file-system-scan-commands))
                          #:linux-modules '#$linux-modules
                          #:linux-module-directory '#$kodir
                          #:keymap-file #+(and=> keyboard-layout
                                                 keyboard-layout->console-keymap)
                          #:qemu-guest-networking? #$qemu-networking?
                          #:volatile-root? '#$volatile-root?
                          #:on-error '#$on-error)))))
   #:name "asahi-initrd"))
