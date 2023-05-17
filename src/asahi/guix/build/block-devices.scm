(define-module (asahi guix build block-devices)
  #:use-module (gnu system uuid)
  #:use-module (guix build syscalls)
  #:use-module (guix records)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:export (block-device
            block-device-block-size
            block-device-label
            block-device-name
            block-device-partuuid
            block-device-type
            block-device-uuid
            block-device?
            block-devices
            make-block-device))

(define-record-type* <block-device>
  block-device make-block-device block-device?
  (block-size block-device-block-size)
  (label block-device-label)
  (name block-device-name)
  (partuuid block-device-partuuid)
  (type block-device-type)
  (uuid block-device-uuid))

(define (alist-block-device-block-size alist)
  (let ((val (assoc-ref alist 'BLOCK_SIZE)))
    (and (string? val) (string->number val))))

(define (alist-block-device-label alist)
  (assoc-ref alist 'LABEL))

(define (alist-block-device-name alist)
  (assoc-ref alist 'DEVNAME))

(define (alist-block-device-partuuid alist)
  (let ((val (assoc-ref alist 'PARTUUID)))
    (and (string? val) (uuid val))))

(define (alist-block-device-type alist)
  (assoc-ref alist 'TYPE))

(define (alist-block-device-uuid alist)
  (let ((val (assoc-ref alist 'UUID))
        (type (alist-block-device-type alist)))
    (when (string? val)
      (cond ((equal? "vfat" type)
             (uuid val 'fat32))
            (else (uuid val))))))

(define (alist->block-device alist)
  (block-device
   (block-size (alist-block-device-block-size alist))
   (label (alist-block-device-label alist))
   (name (alist-block-device-name alist))
   (partuuid (alist-block-device-partuuid alist))
   (type (alist-block-device-type alist))
   (uuid (alist-block-device-uuid alist))))

(define (execute-command command . args)
  (let* ((error-pipe (pipe))
         (port (parameterize ((current-error-port (cdr error-pipe)))
                 (apply open-pipe* OPEN_READ command args)))
         (output (read-string port))
         (status (close-pipe port)))
    (close-port (cdr error-pipe))
    (unless (zero? status)
      (throw 'execute-command-error
             (list status output (read-string (car error-pipe)))))
    output))

(define (split-by-predicate lst predicate)
  (let loop ((lst lst) (acc '()) (out '()))
    (cond
     ((null? lst)
      (reverse (cons (reverse acc) out)))
     ((predicate (car lst))
      (loop (cdr lst) '() (cons (reverse acc) out)))
     (else
      (loop (cdr lst) (cons (car lst) acc) out)))))

(define (parse-line line)
  (let ((parts (string-split line #\=)))
    (when (= 2 (length parts))
      (cons (string->symbol (car parts))
            (cadr parts)))))

(define (parse-export-output s)
  (let ((sections (map parse-line (string-split s #\newline))))
    (remove null-list? (split-by-predicate sections not-pair?))))

(define* (block-devices #:key (blkid "blkid"))
  (let ((output (execute-command blkid "--output" "export")))
    (map alist->block-device (parse-export-output output))))

;; (pretty-print (block-devices))
