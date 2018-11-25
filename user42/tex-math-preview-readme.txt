;;; Commentary:

;; M-x tex-math-preview previews TeX math expressions.  Put point in a TeX,
;; LaTeX, Texinfo, Wikipedia or DBTexMath math expression and M-x
;; tex-math-preview shows either an image or TeX error messages.
;;
;; `tex-mode' has its own far more substantial buffer and region previewing,
;; but tex-math-preview is intentionally simpler and is geared towards
;; unsophisticated TeX users.
;;
;; This started for Texinfo as a way to check how "@tex $$ $$ @end tex"
;; forms come out, and @math{} after it became TeX-ified.  The LaTeX bits
;; haven't had quite as much use, and there might be better ways to choose
;; between TeX or LaTeX.  See also latex-math-preview.el for an adaption
;; designed specifically for that, http://www.emacswiki.org/LaTeXMathPreview
;;
;; See also latex-math-symbol.el (which comes with mu-cite) for just turning
;; TeX character forms such as \alpha into Unicode characters.

