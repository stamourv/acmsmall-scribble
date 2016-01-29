#lang racket/base

;; based heavily on lipics

;; known discrepancies with actual tex-based acmsmall output:
;; - bibliography entries are: "Author. Yeah. Title, ...", autobib can't do that
;; - figures have hrules, and "Figure N" and captions look different
;; - tables don't use the ACM's special commands

(require "download.rkt"
         racket/class
         scribble/base scribble/decode
         (except-in scribble/core paragraph)
         (rename-in scribble/doclang [#%module-begin -#%module-begin])
         scribble/private/defaults
         scribble/html-properties scribble/latex-properties
         setup/main-collects
         setup/collects
         (for-syntax racket/base))

(provide (all-from-out scribble/base)
         (except-out (all-from-out scribble/doclang)
                     -#%module-begin)
         (rename-out [--#%module-begin #%module-begin])
         markboth ccsxml ccsdesc received
         acmsmall-style)

(define ((post-process journal) doc)
  (add-defaults doc
                (string->bytes/utf-8 (format #<<FORMAT
%% Scribble needs these options, so provide before acmsmall
\PassOptionsToPackage{usenames,dvipsnames}{color}
\documentclass[prodmode,~a]{acmsmall}
\bibliographystyle{plain}
FORMAT
journal)
)
                (collection-file-path "style.tex" "scribble" "acmsmall")
                (list acmsmall-class-path acmcopyright-style-path)
                #f))

(define-syntax (--#%module-begin stx)
  (syntax-case stx ()
    [(_ journal ?e ...)
     (quasisyntax/loc stx
       (-#%module-begin doc (post-process '#,(syntax->datum #'journal)) () ?e ...))]))

;; Reader configuration for #lang
(module reader scribble/base/reader
  scribble/acmsmall
  #:wrapper1 (lambda (t) (t)))

;; command wrappers
;; taken from classicthesis-scribble
(define-syntax-rule (define-wrappers (name style) ...)
  (begin
    (define (name . str)
      (make-element (make-style style '()) (decode-content str)))
    ...
    (provide name ...)))
(define-syntax-rule (define-pre-title-wrappers (name style) ...)
  (begin
    (define (name . str)
      (make-paragraph
       (make-style 'pretitle '())
       (make-element
        (make-style style '())
        (decode-content str))))
    ...
    (provide name ...)))
(define-syntax-rule (define-includer name style)
  (begin
    (define-syntax (name stx)
      (syntax-case stx ()
        [(_ module)
         (let ()
           (define name* (gensym 'name))
           #'(begin
               (require (rename-in module [doc name*]))
               (make-nested-flow (make-style style '(command))
                                 (part-blocks name*))))]))
    (provide name)))

(define-wrappers
  [affil                  "affil"]
  [paragraph              "paragraph"]
  [paragraph*             "paragraph*"]
  [acknowledgments        "acmsmallacknowledgments"]
  [set-copyright          "setcopyright"])

(define-pre-title-wrappers
  [acm-volume   "acmVolume"]
  [acm-number   "acmNumber"]
  [acm-article  "acmArticle"]
  [acm-year     "acmYear"]
  [acm-month    "acmMonth"]
  [doi          "doi"]
  [issn         "issn"]
  [abstract     "acmsmallabstract"]
  [author       "author"]
  [keywords     "keywords"]
  [acm-format   "acmformat"] ; see acmsmall docs for what that is
  [bottom-stuff "acmsmallbottomstuff"])
(define-includer include-abstract "acmsmallabstract")

(define (markboth authors name)
  (make-paragraph
   (make-style 'pretitle '())
   (make-multiarg-element (make-style "markboth" '())
                          (list (decode-content (list authors))
                                (decode-content (list name))))))

(define (ccsxml . content) ; see http://dl.acm.org/ccs.cfm for these two
  (make-paragraph
   (make-style 'pretitle '(exact-chars))
   (make-element (make-style #f '(exact-chars))
                 (list "\\begin{CCSXML}\n"
                       content
                       "\n\\end{CCSXML}\n"))))
(define (ccsdesc n . content)
  (make-paragraph
   (make-style 'pretitle '(exact-chars))
   (make-multiarg-element (make-style "acmsmallccsdesc" '(exact-chars))
                          (list (decode-content (list (number->string n)))
                                content))))

(define (received x1 x2 x3)
  (make-paragraph
   (make-style #f '())
   (make-multiarg-element (make-style "acmsmallreceived" '())
                          (list (decode-content (list x1))
                                (decode-content (list x2))
                                (decode-content (list x3))))))


;; Bibliography setup
(define autobib-style-extras
  (let ([abs (lambda (s)
               (path->main-collects-relative
                (collection-file-path s "scriblib")))])
    (list
     (make-css-addition (abs "autobib.css"))
     (make-tex-addition (abs "autobib.tex")))))

(define bib-single-style (make-style "AutoBibliography" autobib-style-extras))
(define bibentry-style (make-style "Autobibentry" autobib-style-extras))
(define colbibnumber-style (make-style "Autocolbibnumber" autobib-style-extras))
(define colbibentry-style (make-style "Autocolbibentry" autobib-style-extras))

(define acmsmall-style
  (new
   (class object%
     (define/public (bibliography-table-style) bib-single-style)
     (define/public (entry-style) colbibentry-style)
     (define/public (disambiguate-date?) #f)
     (define/public (collapse-for-date?) #f)
     (define/public (get-cite-open) "[")
     (define/public (get-cite-close) "]")
     (define/public (get-group-sep) "; ")
     (define/public (get-item-sep) ", ")
     (define/public (render-citation date-cite i) date-cite)
     (define/public (render-author+dates author dates) (list* author " " dates))
     (define (make-label i)
       (string-append "autobiblab:" (number->string i)))
     (define/public (bibliography-line i e)
       (list e))
     (super-new))))


;; Download necessary style files
(download-acmsmall-files)
