#lang racket/base

;; based heavily on lipics

(require "download.rkt"
         scribble/base scribble/decode
         (except-in scribble/core paragraph)
         (rename-in scribble/doclang [#%module-begin -#%module-begin])
         scribble/private/defaults
         (for-syntax racket/base))

(provide (all-from-out scribble/base)
         (except-out (all-from-out scribble/doclang)
                     -#%module-begin)
         (rename-out [--#%module-begin #%module-begin])
         markboth ccsxml ccsdesc)

;; header mostly taken from the lipics sample article
(define (post-process doc)
  (add-defaults doc
                ;; FIXME: allow configuration
                (string->bytes/utf-8 #<<FORMAT
%% Scribble needs these options, so provide before toplas
\PassOptionsToPackage{usenames,dvipsnames}{color}
\documentclass[prodmode,acmtoplas]{acmsmall}
\setcopyright{rightsretained}
FORMAT
)
                (collection-file-path "style.tex" "scribble" "toplas")
                (list acmsmall-class-path acmcopyright-style-path)
                #f))

(define-syntax (--#%module-begin stx)
  (syntax-case stx ()
    [(_ ?e ...)
     (quasisyntax/loc stx
       (-#%module-begin doc post-process () ?e ...))]))

;; Reader configuration for #lang
(module reader scribble/base/reader
  scribble/toplas
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
  [subject-classification "subjclass"]
  [terms                  "terms"]
  [keywords               "keywords"]
  [paragraph              "paragraph"]
  [paragraph*             "paragraph*"]
  [acknowledgments        "acknowledgments"])

(define-pre-title-wrappers
  [acm-volume  "acmVolume"]
  [acm-number  "acmNumber"]
  [acm-article "acmArticle"]
  [acm-year    "acmYear"]
  [acm-month   "acmMonth"]
  [doi         "doi"]
  [issn        "issn"]
  [abstract    "toplasabstract"]
  [author      "author"]
  )
(define-includer include-abstract "toplasabstract")

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
   (make-multiarg-element (make-style "toplasccsdesc" '(exact-chars))
                          (list (decode-content (list (number->string n)))
                                content))))


;; Download necessary style files
(download-acmsmall-files)
