#lang racket/base

;; Notice
;; To install (from within the package directory):
;;   $ raco pkg install
;; To install (once uploaded to pkgs.racket-lang.org):
;;   $ raco pkg install <<name>>
;; To uninstall:
;;   $ raco pkg remove <<name>>
;; To view documentation:
;;   $ raco docs <<name>>
;;
;; For your convenience, we have included LICENSE-MIT and LICENSE-APACHE files.
;; If you would prefer to use a different license, replace those files with the
;; desired license.
;;
;; Some users like to add a `private/` directory, place auxiliary files there,
;; and require them in `main.rkt`.
;;
;; See the current version of the racket style guide here:
;; http://docs.racket-lang.org/style/index.html

(require txexpr pollen/tag pollen/setup rackunit)
(provide (all-defined-out))

;; utilities

(define (get-attr x xs) (cadr (assq x xs)))

;; p feature
;; no attributes needed
;; md/txt: blank line before & after
;; html enclose content in <p></p> tags
;; ltx/pdf: no special markup

(define-tag-function (p attributes elements)
  (case (current-poly-target)
    [(txt md) (apply string-append `(,@elements))]
    [(html) `(p ,@elements)]
    [(pdf ltx) (apply string-append `(,@elements "\\newline"))]
    [else (txexpr 'p empty elements)]
    )
  )


;; hrule feature

(define (hrule . elements)
  (case (current-poly-target)
    [(txt) "----------------------------------------"]
    [(html) "<hr>"]
    [(pdf ltx) "\noindent\\makebox[\\linewidth]{\\rule{12cm}{0.4pt}}"]
    [else (txexpr 'hrule empty empty)]
    ))

;; figure feature

(define-tag-function (figure attributes elements)
  (define path (cadr (assq 'path attributes)))
  (case (current-poly-target)
    [(txt) elements]
    [(html) (apply string-append `("<img src='" ,path " alt='" ,@elements "/>"))]
    [(pdf ltx) (apply string-append `("\\begin{figure}\\includegraphics[totalheight=8cm]{" ,path "}\\caption{" ,@elements"}\\end{figure}"))]
    [else (txexpr 'figure attributes elements)]
    ))

;; litem (list item) feature

(define-tag-function (litem attributes elements)
  (case (current-poly-target)
    [(txt) (apply string-append `(,@elements))]
    [(html) `(li ,@elements)]
    [(pdf ltx) (apply string-append `("\\item " ,@elements))]
    [else (txexpr 'litem empty elements)]))

;; list feature

(define tlist-ltx-tag
  (hash
   'ul "itemize"
   'ol "enumerate"
   )
  )

(define (leave-irrelevant xs)
  (filter (lambda (x) (not(equal? x "\n"))) xs)
  )

(define (ol-elems->txt elements)
  (define xs (leave-irrelevant elements))
  (for/list ([x xs]
             [i (in-naturals 1)])
    (string-append (number->string i) ". " x "\n")))

(define (ul-elems->txt elements)
  (define xs (leave-irrelevant elements))
  (for/list ([x xs])
    (string-append " * " x "\n")
    )
  )

(define (elems->txt ordering elements)
  (case ordering
    [(ul) (ul-elems->txt elements)]
    [(ol) (ol-elems->txt elements)]
    [else elements]
    )
  )

(define-tag-function (tlist attributes elements)
  (define ordering (string->symbol (get-attr 'ordering attributes)))
  (define ltx-ordering (hash-ref tlist-ltx-tag ordering))
  (case (current-poly-target)
    [(txt md) (elems->txt ordering elements)]
    [(html) `(,ordering ,@elements)]
    [(ltx pdf)
     (apply string-append
            `("\\begin{" ,ltx-ordering "}"
                         ,@elements
                         "\\end{" ,ltx-ordering "}"))]
    (else (txexpr 'list empty elements))))

;; box feature

(define-tag-function (tbox attrs elements)
  (define label (get-attr 'label attrs))
  (define label-string (if label (string-append "\\textbf{" label "}\\newline") ""))
  (case (current-poly-target)
    [(txt) elements]
    [(pdf ltx)
     (apply string-append
      `("\\fbox{\\newline"
      "\\begin{minipage}{\\textwidth}\\newline"
      ,label-string
      ,@elements
      "\\end{minipage}}\\newline"))]
    [else `(box ,@attrs ,@elements)]))

;; heading feature

(define-tag-function (heading attrs elements)
  (define h-level (cadr (assq 'level attrs)))
  (define h-tag (string->symbol (format "h~a" h-level)))
  (define heading-commands
    (hash
     1 "\\chapter"
     2 "\\section"
     3 "\\subsection"
     4 "\\subsubsection"
     5 "\\paragraph"
     6 "\\subparagraph")
    )
  (define h-ltx (hash-ref heading-commands h-level))
  (case (current-poly-target)
    [(txt) `(heading ,@elements)]
    [(html) `(,h-tag ,@elements)]
    [(pdf ltx) (apply string-append `(,h-ltx "{" ,@elements "}"))]
    [else `(heading ,@attrs ,@elements)]
    )
  )
