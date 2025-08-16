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

(require txexpr pollen/tag pollen/setup)
(provide (all-defined-out))

;; p feature

(define (p . elements)
  (case (current-poly-target)
    [(txt) elements]
    [(html) (apply string-append `("<p>" ,@elements "</p>"))]
    [(pdf ltx) (apply string-append `(,@elements))]
    [else (txexpr 'p empty elements)]
    )
  )


;; hrule feature

(define (hrule . elements)
  (case (current-poly-target)
    [(txt) "----------------------------------------"]
    [(html) "<hr>"]
    [(pdf ltx) "\\noindent\\makebox[\\linewidth]{\\rule{12cm}{0.4pt}}"]
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

(define (litem . elements)
  (case (current-poly-target)
    [(txt) (apply string-append elements)]
    [(html) (apply string-append `("<li>" ,@elements "</li>"))]
    [(pdf ltx) (apply string-append `("\\item " ,@elements))]
    [else (txexpr 'litem empty elements)]))

;; list feature

(define (tlist . elements)
  (case (current-poly-target)
    [(txt) (string-append (for/list ([e elements]) (string-append e "\n")))]
    [(html) (apply string-append `("<ul>" ,@elements "</ul>"))]
    [(ltx pdf)
     (apply string-append
      `("\\begin{enumerate}\n"
      ,@elements
      "\\end{enumerate}\n"))]
    (else (txexpr 'list empty elements))))

;; box feature

(define-tag-function (tbox attrs elements)
  (define label (cadr (assq 'label attrs)))
  (define label-string (if label (string-append "\\textbf{" label "}\n\n") ""))
  (case (current-poly-target)
    [(txt) elements]
    [(pdf ltx)
     (apply string-append
      `("\\fbox{\n"
      "\\begin{minipage}{\\textwidth}\n"
      ,label-string
      ,@elements
      "\n\\end{minipage}}\n"))]
    [else `(box ,@attrs ,@elements)]))

;; heading feature

(define-tag-function (heading attrs elements)
  (case (current-poly-target)
    [(txt) `(heading ,@elements)]
    [(html) `(heading ,@elements)]
    [(pdf ltx) (apply string-append `("\n\\large{" ,@elements "}\n"))]
    [else `(heading ,@attrs ,@elements)]
    )
  )