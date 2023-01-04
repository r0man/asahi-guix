(define-module (asahi initrd)
  #:use-module (asahi firmware)
  #:use-module (asahi packages)
  #:use-module (gnu build linux-modules)
  #:use-module (gnu packages linux)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system keyboard)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system mapped-devices)
  #:use-module (guix gexp)
  #:use-module (guix modules)
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
                       (helper-packages '())
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

  ;; (format #t "LINUX: ~a\n" linux)
  ;; (format #t "HELPER: ~a\n" helper-packages)
  ;; (format #t "FILE SYSTEMS: ~a\n" file-systems)
  ;; (format #t "MAPPED DEVICES: ~a\n" mapped-devices)
  ;; (format #t "DEVICE MAPPING COMMANDS: ~a\n" device-mapping-commands)

  (expression->initrd
   (with-extensions (list asahi-guix)
     (with-imported-modules (source-module-closure
                             '((asahi firmware)
                               (gnu build file-systems)
                               (gnu build linux-boot)
                               (gnu build linux-modules)
                               (gnu system file-systems)
                               (guix build bournish)
                               (guix build utils)))
       #~(begin
           (use-modules (asahi firmware)
                        (gnu build linux-boot)
                        (gnu system file-systems)
                        ((guix build utils) #:hide (delete))
                        (guix build bournish)   ;add the 'bournish' meta-command
                        (srfi srfi-1)           ;for lvm-device-mapping
                        (srfi srfi-26)
                        (gnu build linux-modules)
                        ;; FIXME: The following modules are for
                        ;; LUKS-DEVICE-MAPPING.  We should instead propagate
                        ;; this info via gexps.
                        ((gnu build file-systems)
                         #:select (find-partition-by-luks-uuid))
                        (rnrs bytevectors))

           (display ":: Asahi: Triggering early load of NVMe modules...\n")
           (load-linux-modules-from-directory '("apple-mailbox" "nvme-apple") '#$kodir)

           (with-output-to-port (%make-void-port "w")
             (lambda ()
               (set-path-environment-variable "PATH" '("bin" "sbin")
                                              '#$helper-packages)))

           (parameterize ((current-warning-port (%make-void-port "w")))
             (boot-system #:mounts
                          (map spec->file-system
                               '#$(map file-system->spec file-systems))
                          #:pre-mount (lambda ()
                                        (mount-efi-system-partition "/run/.system-efi")
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
