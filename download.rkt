#lang racket/base

;; Provides helpers for downloading acmsmall style files
;; (mostly taken from the lipics package)

(require file/md5
         file/unzip
         net/url
         racket/file
         racket/port)

(provide acmsmall-class-path
         acmcopyright-style-path
         download-acmsmall-files)

(define acmsmall-url  "http://www.acm.org/publications/article-templates/acmsmall.zip")
(define acmsmall-hash #"7458f15b11f21d89ab5aae19aa0e0179")

(define acmsmall-base-path
  (build-path (find-system-path 'addon-dir) "acmsmall-style-files"))
(define acmsmall-class-path (build-path acmsmall-base-path "acmsmall.cls"))
(define acmcopyright-style-path (build-path acmsmall-base-path "acmcopyright.sty"))

;; Download acmsmall class file to the add-on directory
(define (download-acmsmall-files)
  (unless (directory-exists? acmsmall-base-path)
    (define tmp (make-temporary-file))
    (displayln (format "Downloading class file via ~a" acmsmall-url))
    (define out (open-output-file tmp #:exists 'truncate))
    (call/input-url (string->url acmsmall-url)
                    get-pure-port
                    (λ (in) (copy-port in out)))
    (close-output-port out)
    (define hash
      (with-input-from-file tmp
        (λ () (md5 (current-input-port) #t))))
    (unless (equal? hash acmsmall-hash)
      (raise-arguments-error 'scribble/toplas
                             "Invalid MD5 hash for acmsmall tarball"
                             "expected" acmsmall-hash
                             "given" hash))
    ;; Don't make the directory until we have a valid download
    (make-directory acmsmall-base-path)
    (unzip tmp (make-filesystem-entry-reader #:dest acmsmall-base-path))))
