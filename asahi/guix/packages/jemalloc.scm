(define-module (asahi guix packages jemalloc)
  #:use-module ((gnu packages jemalloc) #:prefix gnu:)
  #:use-module ((guix licenses) #:select (bsd-2))
  #:use-module (gnu packages perl)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (replace-jemalloc))

(define-public jemalloc-4.5.0
  (package
    (name "jemalloc")
    (version "4.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/jemalloc/jemalloc/releases/download/"
                    version "/jemalloc-" version ".tar.bz2"))
              (sha256
               (base32
                "10373xhpc10pgmai9fkc1z0rs029qlcb3c0qfnvkbwdlcibdh2cl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-thp-test
           ;; This test does not check if transparent huge pages are supported
           ;; on the system before running the test.
           (lambda _
             (substitute* "Makefile.in"
               (("\\$\\(srcroot\\)test/unit/pages.c \\\\") "\\"))
             #t)))
       #:configure-flags
       '(,@(match (%current-system)
             ((or "aarch64-linux" "i686-linux" "x86_64-linux")
              '())
             ("powerpc-linux"
              (list "--disable-thp" "CPPFLAGS=-maltivec"))
             (_
              (list "--disable-thp"))))))
    ;; Install the scripts to a separate output to avoid referencing Perl and
    ;; Bash in the default output, saving ~75 MiB on the closure.
    (outputs '("out" "bin"))
    (home-page "http://jemalloc.net/")
    (synopsis "General-purpose scalable concurrent malloc implementation")
    (description
     "This library providing a malloc(3) implementation that emphasizes
fragmentation avoidance and scalable concurrency support.")
    (license bsd-2)))

(define-public jemalloc
  (package
    (inherit jemalloc-4.5.0)
    (version "5.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/jemalloc/jemalloc/releases/download/"
                    version "/jemalloc-" version ".tar.bz2"))
              (sha256
               (base32
                "1apyxjd1ixy4g8xkr61p0ny8jiz8vyv1j0k4nxqkxpqrf4g2vf1d"))))
    (arguments
     (substitute-keyword-arguments (package-arguments jemalloc-4.5.0)
       ;; Disable the thread local storage model in jemalloc 5 to prevent
       ;; shared libraries linked to libjemalloc from crashing on dlopen()
       ;; https://github.com/jemalloc/jemalloc/issues/937
       ((#:configure-flags base-configure-flags '())
        `(cons "--disable-initial-exec-tls" ,base-configure-flags))))
    (inputs (list perl))))

(define replace-jemalloc
  (package-input-rewriting `((,gnu:jemalloc . ,jemalloc))))
