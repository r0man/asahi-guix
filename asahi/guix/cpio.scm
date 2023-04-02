(define-module (asahi guix cpio)
  #:use-module (guix build utils)
  #:use-module (guix cpio)
  #:use-module (ice-9 binary-ports)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:export (extract-cpio-archive))

(define (char-null? c)
  (eq? #\null c))

(define (last-section? name)
  (equal? "TRAILER!!!" name))

(define cpio-header-file-size
  (@@ (guix cpio) cpio-header-file-size))

(define cpio-header-name-size
  (@@ (guix cpio) cpio-header-name-size))

(define cpio-header-mode
  (@@ (guix cpio) cpio-header-mode))

(define mode->type
  (@@ (guix cpio) mode->type))

(define (cpio-header-type header)
  (mode->type (cpio-header-mode header)))

(define (padding offset)
  (modulo (- 4 (modulo offset 4)) 4))

(define (read-padding offset port)
  (get-bytevector-n port (padding offset)))

(define (read-name port header)
  (let* ((size (cpio-header-name-size header))
         (name (utf8->string (get-bytevector-n port size))))
    (read-padding (+ 110 size) port)
    (string-trim-right name char-null?)))

(define (extract-cpio-directory in header target)
  (let* ((file-size (cpio-header-file-size header))
         (name (read-name in header))
         (dirname (string-append target "/" name)))
    (get-bytevector-n in file-size)
    (read-padding file-size in)
    (mkdir-p dirname)
    (chmod dirname (cpio-header-mode header))
    `((type . ,(cpio-header-type header))
      (name . ,dirname)
      (size . ,file-size))))

(define (extract-cpio-regular in header target)
  (let* ((file-size (cpio-header-file-size header))
         (name (read-name in header))
         (filename (string-append target "/" name)))
    (when (not (last-section? name))
      (mkdir-p (dirname filename))
      (call-with-output-file filename
        (lambda (out)
          (put-bytevector out (get-bytevector-n in file-size)))
        #:binary #t)
      (chmod filename (cpio-header-mode header))
      (read-padding file-size in)
      `((type . ,(cpio-header-type header))
        (name . ,filename)
        (size . ,file-size)))))

(define (extract-cpio-section in target)
  (let ((header (read-cpio-header in)))
    (case (cpio-header-type header)
      ((regular)
       (extract-cpio-regular in header target))
      ((directory)
       (extract-cpio-directory in header target))
      (else
       (error "file type not supported")))))

(define (extract-cpio-sections in target)
  (let ((section (extract-cpio-section in target)))
    (if (list? section)
        (cons section (extract-cpio-sections in target))
        '())))

(define (extract-cpio-archive archive target)
  (call-with-input-file archive
    (lambda (in)
      (set-port-encoding! in "ISO-8859-1")
      (extract-cpio-sections in target))
    #:binary #t))

;; (extract-cpio-archive "firmware.cpio" "/tmp/firmware")
