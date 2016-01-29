#lang scribble/manual

@(require (for-label scribble/acmsmall
                     scribble/decode))

@title{@tt{acmsmall} support for Scribble}

@(defmodule scribble/acmsmall #:lang)

The @racketmodname[scribble/acmsmall] language provides support for the
@hyperlink["http://www.acm.org/publications/article-templates/acm-latex-style-guide"]{@tt{acmsmall}}
paper format, used by various ACM journals.

The language provides all of the bindings of @racketmodname[scribble/base]
in addition to additional procedures for supporting @tt{acmsmall}-specific
typesetting.

To select which journal to format for, the first expression after the
@racketid{#lang} line should be the journal identifier. For example, when
formatting for TOPLAS, use @racketid{#lang scribble/acmsmall acmtoplas}.
The complete list of identifiers is available in the @tt{acmsmall} style
documentation available through the above link.

If using the @racketmodname[scriblib/autobib] library, ensure that the
@racket[define-cite] form is used with the @racket[acmsmall-style] setting
for @racket[#:style].

Also see the
@(hyperlink "https://github.com/stamourv/acmsmall-scribble/blob/master/example.scrbl"
            "example document")
to get started.


@defproc[(author [name pre-content?] [affil pre-content?] ... ...)
         paragraph?]{
  Registers the authors of the paper.
  The arguments should be an alternating list of author names and affiliations
  (created by @racket[affil]).
}
@defproc[(affil [pre-content pre-content?])
         element?]{
  Registers the affiliation of a given author.
}

@defproc[(abstract [pre-content pre-content?] ...) paragraph?]{
  Typesets an abstract.
}
@defform[(include-abstract abstract-path)]{
  Includes the contents of the file at @racket[abstract-path] as abstract.
}

@defproc[(paragraph [pre-content pre-content?] ...) element?]{
  Typesets a paragraph.
}
@defproc[(paragraph* [pre-content pre-content?] ...) element?]{
  Typesets a paragraph.
}

@defproc[(acknowledgments [pre-content pre-content?] ...)
         element?]{
  Typesets an acknowledgments section. This will normally go in the last
  section of a document. Note that @racket[acknowledgments] will not typeset
  as expected if it is used outside of a section, such as in the top-level
  of a document that uses @racket[include-section].
}

@deftogether[(@defproc[(set-copyright [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-volume [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-number [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-article [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-year [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-month [pre-content pre-content?] ...) paragraph?]
              @defproc[(doi [pre-content pre-content?] ...) paragraph?]
              @defproc[(issn [pre-content pre-content?] ...) paragraph?]
              @defproc[(keywords [pre-content pre-content?] ...) paragraph?]
              @defproc[(acm-format [pre-content pre-content?] ...) paragraph?]
              @defproc[(bottom-stuff [pre-content pre-content?] ...) paragraph?]
              @defproc[(ccsxml [pre-content pre-content?] ...) paragraph?]
              @defproc[(ccsdesc [pre-content pre-content?] ...) paragraph?]
)]{
Simple wrappers over the corresponding TeX macros in the @tt{acmsmall} style.
Consult the @tt{acmsmall} style documentation for details.
}

@defproc[(markboth [authors pre-content?] [title pre-content?]) paragraph?]{
Sets the running head. See the @tt{acmsmall} style documentation for formatting
details.
}

@defproc[(received [received-date pre-content?] [revised-date pre-content?] [accepted-date pre-content?]) paragraph?]{
Sets various relevant dates. Must be in the last section of the paper.
}

@defthing[acmsmall-style any/c]{
  A value used to customize bibliography generation for the paper.
  Provide this as an argument for the @racket[define-cite] form using
  the @racket[#:style] keyword.
}
