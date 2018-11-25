;;; latex-pretty-symbols.el --- Display many latex symbols as their unicode counterparts

;; Copyright (C) 2011 Erik Parmann, PÂl Drange
;;
;; Author: Erik Parmann <eparmann@gmail.com>
;;         PÂl Drange
;; Created: 10. July 2011
;; Version: 1.0
;; Keywords: convenience, display
;; Url: https://bitbucket.org/mortiferus/latex-pretty-symbols.el
;; Derived from  pretty-lambda.el (http://www.emacswiki.org/emacs/PrettyLambda ) by Drew Adams

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; License:
;; Licensed under the same terms as Emacs.

;;; Commentary:
;; Description: This library use font-locking to change the way Emacs displays
;;   various latex commands, like \Gamma, \Phi, etc.  It does not actually
;;   change the content of the file, only the way it is displayed.
;;
;; Quick start:
;;   add this file to load path, then (require 'latex-pretty-symbols)
;;

;;; TODO: The most pressing isue is a way to let not only single symbols be
;;   displayed, but also strings.  Then we can e.g display "ËËCÈÈ" instead of
;;   atldiamond.  Currently the 5 symbols gets placed on top of each other,
;;   resulting in a mighty mess.  This problem might be demomposed into two
;;   types: When the replaced string is bigger than the string replacing it
;;   (probably the easiest case), and the converse case.
;;
;;   Package it as a elpa/marmelade package.
;;
;;   Also it would be nice if it had some configuration possibilities. Eg the
;;   ability to add own abreviations through the customization interface, or
;;   toggle the display of math-curly-braces.
;;
;;   On a longer timeline, it would be nice if it could understand some basic
;;   newcommands, and automatically deduce the needed unicode (but this seems
;;   super hard).


;;; Code:
(defun substitute-pattern-with-unicode-symbol (pattern symbol)
  "Add a font lock hook to replace the matched part of PATTERN with the Unicode
symbol SYMBOL.
Symbol can be the symbol directly, no lookup needed."
  (interactive)
  (font-lock-add-keywords
   nil
   `((,pattern
      (0 (progn
	   ;;Non-working section kind of able to compose multi-char strings:
	   ;; (compose-string-to-region (match-beginning 1) (match-end 1)
	   ;; 				  ,symbol
	   ;; 				  'decompose-region)
	   ;; (put-text-property (match-beginning 1) (match-end 1) 'display ,symbol)
	   (compose-region (match-beginning 1) (match-end 1)
	   		   ,symbol
	   		   'decompose-region)
	   nil))))))

;;The following code can be used to add strings, and not just single
;; characters. But not really working yet, as it does not remove the thing that
;; is below
;; (require 'cl)
;; (defun compose-string-to-region (start end string decomposingfunct)
;;   (loop for i from 0 to (- (length string) 1)
;; 	do (compose-region (+ i start) (+  start i 1) (substring string i (+ i 1) ) decomposingfunct)))


(defun substitute-patterns-with-unicode-symbol (patterns)
  "Mapping over PATTERNS, calling SUBSTITUTE-PATTERN-WITH-UNICODE for each of the patterns."
  (mapcar #'(lambda (x)
              (substitute-pattern-with-unicode-symbol (car x)
						      (second x)))
          patterns))

(defun latex-escape-regex (str)
  "Gets a string, e.g. Alpha, returns the regexp matching the escaped
version of it in LaTeX code, with no chars in [a-z0-9A-Z] after it."
  (interactive "MString:")
  (concat "\\(\\\\" str "\\)[^a-z0-9A-Z]"))


(defun latex-escape-regexp-super-or-sub (str sup-or-sub backslash)
  "Gets a string, e.g. 1, a symbol 'sub or 'sup saying wether it
should be a superscript or subscript, and a boolean for
  backslash saying wether or not str should be backslashed (like
  \gamma). It returns the regexp matching the super/sub-scripted
  version of it in LaTeX code"
  ;; We can not use "(regexp-opt (list (concat "_" str) (concat "_{" str
  ;; "}")))", as it adds a "?:" after every group, which eveidently breaks the
  ;; font-locking engine in emacs. I assume the reason is like this: Normaly a
  ;; group (denoted with paranthesises) create a "backreference". This makes it
  ;; possible (I think) for the font-locking-engine no know where it actually
  ;; matched. The "?:" sais that we need to backreference. We have added one in
  ;; the innermost group, as we never need to know the location of the inner
  ;; matching, only the outermost. Adding "?:" where it can be done makes
  ;; matching more effecient, as the engine dont need to make a backreference
  (if backslash (setf str (concat "\\\\" str)))
  (case sup-or-sub
    ('sub  (concat "\\(_\\(?:" str "\\|{" str "}\\)\\)"))
    ('sup  (concat "\\(\\^\\(?:" str "\\|{" str "}\\)\\)"))))

(defun latex-escape-regex-sub (str &optional backslash)
  "Gets a string, e.g. 1, returns the regexp matching the subscripted
version of it in LaTeX code."
  (interactive "MString:")
  (latex-escape-regexp-super-or-sub str 'sub backslash))

(defun latex-escape-regex-sup (str &optional backslash)
  "Gets a string, e.g. 1, returns the regexp matching the superscripted
version of it in LaTeX code."
  (interactive "MString:")
  (latex-escape-regexp-super-or-sub str 'sup backslash))

;;Goto http://www.fileformat.info/info/unicode/block/mathematical_operators/list.htm and copy the needed character
(defun latex-unicode-simplified ()
  "Adds a bunch of font-lock rules to display latex commands as
their unicode counterpart"
  (interactive)
  (substitute-patterns-with-unicode-symbol
   (list
    ;;These need to be on top, before the versions which are not subscriptet
    (list (latex-escape-regex-sub "beta" t)"f")
    (list (latex-escape-regex-sub "gamma" t)"g")
    (list (latex-escape-regex-sub "rho" t)"h")
    (list (latex-escape-regex-sub "phi" t)"i")
    (list (latex-escape-regex-sub "chi" t)"j")
    
    (list (latex-escape-regex "Alpha") "ë")
    (list (latex-escape-regex "Beta") "í")
    (list (latex-escape-regex "Gamma")"ì")
    (list (latex-escape-regex "Delta")"î")
    (list (latex-escape-regex "Epsilon")"ï")
    (list (latex-escape-regex "Zeta")"ñ")
    (list (latex-escape-regex "Eta")"ó")
    (list (latex-escape-regex "Theta")"ò")
    (list (latex-escape-regex "Iota")"ô")
    (list (latex-escape-regex "Kappa")"ö")
    (list (latex-escape-regex "Lambda")"õ")
    (list (latex-escape-regex "Mu")"ú")
    (list (latex-escape-regex "Nu")"ù")
    (list (latex-escape-regex "Xi")"û")
    (list (latex-escape-regex "Omicron")"ü")
    (list (latex-escape-regex "Pi")"†")
    (list (latex-escape-regex "Rho")"°")
    (list (latex-escape-regex "Sigma")"£")
    (list (latex-escape-regex "Tau")"§")
    (list (latex-escape-regex "Upsilon")"•")
    (list (latex-escape-regex "Phi")"¶")
    (list (latex-escape-regex "Chi")"ß")
    (list (latex-escape-regex "Psi")"®")
    (list (latex-escape-regex "Omega")"©")
    (list (latex-escape-regex "alpha")"±")
    (list (latex-escape-regex "beta")"≤")
    (list (latex-escape-regex "gamma")"≥")
    (list (latex-escape-regex "delta")"¥")
    (list (latex-escape-regex "epsilon")"µ")
    (list (latex-escape-regex "zeta")"∂")
    (list (latex-escape-regex "eta")"∑")
    (list (latex-escape-regex "theta")"∏")
    (list (latex-escape-regex "iota")"π")
    (list (latex-escape-regex "kappa")"∫")
    (list (latex-escape-regex "lambda")"ª")
    (list (latex-escape-regex "mu")"º")
    (list (latex-escape-regex "nu")"Ω")
    (list (latex-escape-regex "xi")"æ")
    (list (latex-escape-regex "omicron")"ø")
    (list (latex-escape-regex "pi")"¿")
    (list (latex-escape-regex "rho")"¡")
    (list (latex-escape-regex "sigma")"√")
    (list (latex-escape-regex "tau")"ƒ")
    (list (latex-escape-regex "upsilon")"≈")
    (list (latex-escape-regex "phi")"∆")
    (list (latex-escape-regex "chi")"«")
    (list (latex-escape-regex "psi")"»")
    (list (latex-escape-regex "omega")"…")

    ;; relations
    (list (latex-escape-regex "geq")"e")
    (list (latex-escape-regex "leq")"d")
    (list (latex-escape-regex "neq")"`")

    ;; logical ops
    (list (latex-escape-regex "land")"'")
    (list (latex-escape-regex "lor")"(")
    (list (latex-escape-regex "neg")"¨")
    (list (latex-escape-regex "rightarrow")"í")
    (list (latex-escape-regex "leftarrow")"ê")
    (list (latex-escape-regex "leftrightarrow")"î")
    (list (latex-escape-regex "Rightarrow")"“")
    (list (latex-escape-regex "Leftarrow")"–")
    (list(latex-escape-regex "Leftrightarrow")"‘")
    (list (latex-escape-regex "forall")" ")
    (list (latex-escape-regex "exists")"")
    (list (latex-escape-regex "Diamond")"ƒ")
    (list (latex-escape-regex "Box")"°")
    (list (latex-escape-regex "models")"ß")
    (list (latex-escape-regex "bot")"•")
    (list (latex-escape-regex "top")"§")
    
    ;; infty before in
    (list (latex-escape-regex "infty")"")
    
    ;; set ops
    ;;Here I have chosen to have \} display as ¨, to easy reading of set building opperations. Slightly uncertain
    (list "\\(\\\\}\\)" "¨")
    (list "\\(\\\\{\\)" "®")
    
    (list (latex-escape-regex "mid")"|")
    (list (latex-escape-regex "in")"
")
    (list (latex-escape-regex "notin")"	")
    (list (latex-escape-regex "cup")"*")
    (list (latex-escape-regex "cap")")")
    (list (latex-escape-regex "setminus")"")
    (list (latex-escape-regex "minus")"")
    (list (latex-escape-regex "subseteq")"Ü")
    (list (latex-escape-regex "subset")"Ç")
    (list (latex-escape-regex "emptyset")"")
    (list (latex-escape-regex "ni")"")
    
    ;; generic math
    (list (latex-escape-regex "dots")"&")
    
    ;;Superscript
    (list (latex-escape-regex-sup "0")"p")
    (list (latex-escape-regex-sup "1")"π")
    (list (latex-escape-regex-sup "2")"≤")
    (list (latex-escape-regex-sup "3")"≥")
    (list (latex-escape-regex-sup "4")"t")
    (list (latex-escape-regex-sup "5")"u")
    (list (latex-escape-regex-sup "6")"v")
    (list (latex-escape-regex-sup "7")"w")
    (list (latex-escape-regex-sup "8")"x")
    (list (latex-escape-regex-sup "9")"y")
    (list (latex-escape-regex-sup "-")"{")
    (list (latex-escape-regex-sup "=")"|")
    (list (latex-escape-regex-sup "\\+")"z")
    (list (latex-escape-regex-sup "a")"C")
    (list (latex-escape-regex-sup "b")"G")
    (list (latex-escape-regex-sup "c")"ú")
    (list (latex-escape-regex-sup "d")"H")
    (list (latex-escape-regex-sup "e")"I")
    (list (latex-escape-regex-sup "f")"†")
    (list (latex-escape-regex-sup "g")"M")
    (list (latex-escape-regex-sup "h")"∞")
    (list (latex-escape-regex-sup "i")"q")
    (list (latex-escape-regex-sup "j")"≤")
    (list (latex-escape-regex-sup "k")"O")
    (list (latex-escape-regex-sup "l")"·")
    (list (latex-escape-regex-sup "m")"P")
    (list (latex-escape-regex-sup "n")"")
    (list (latex-escape-regex-sup "o")"R")
    (list (latex-escape-regex-sup "p")"V")
    (list (latex-escape-regex-sup "r")"≥")
    (list (latex-escape-regex-sup "s")"‚")
    (list (latex-escape-regex-sup "t")"W")
    (list (latex-escape-regex-sup "u")"X")
    (list (latex-escape-regex-sup "v")"[")
    (list (latex-escape-regex-sup "w")"∑")
    (list (latex-escape-regex-sup "x")"„")
    (list (latex-escape-regex-sup "y")"∏")
    (list (latex-escape-regex-sup "z")"ª")
    
    (list (latex-escape-regex-sup "A")",")
    (list (latex-escape-regex-sup "B")".")
    (list (latex-escape-regex-sup "D") "0")
    (list (latex-escape-regex-sup "E") "1")
    (list (latex-escape-regex-sup "G") "3")
    (list (latex-escape-regex-sup "H") "4")
    (list (latex-escape-regex-sup "I") "5")
    (list (latex-escape-regex-sup "J") "6")
    (list (latex-escape-regex-sup "K") "7")
    (list (latex-escape-regex-sup "L") "8")
    (list (latex-escape-regex-sup "M") "9")
    (list (latex-escape-regex-sup "N") ":")
    (list (latex-escape-regex-sup "O") "<")
    (list (latex-escape-regex-sup "P") ">")
    (list (latex-escape-regex-sup "R") "?")
    (list (latex-escape-regex-sup "T") "@")
    (list (latex-escape-regex-sup "U") "A")
    (list (latex-escape-regex-sup "V") "}")
    (list (latex-escape-regex-sup "W") "B")
    
    
    
    ;;Subscripts, unfortunately we lack important part of the subscriptet alphabet, most notably j and m
    (list (latex-escape-regex-sub "1")"Å")
    (list (latex-escape-regex-sub "2")"Ç")
    (list (latex-escape-regex-sub "3")"É")
    (list (latex-escape-regex-sub "4")"Ñ")
    (list (latex-escape-regex-sub "5")"Ö")
    (list (latex-escape-regex-sub "6")"Ü")
    (list (latex-escape-regex-sub "7")"á")
    (list (latex-escape-regex-sub "8")"à")
    (list (latex-escape-regex-sub "9")"â")
    (list (latex-escape-regex-sub "x")"ì")
    (list (latex-escape-regex-sub "i")"b")
    (list (latex-escape-regex-sub "\\+")"ä")
    (list (latex-escape-regex-sub "-")"ã")
    (list (latex-escape-regex-sub "=")"å")
    (list (latex-escape-regex-sub "a")"ê")
    (list (latex-escape-regex-sub "e")"ë")
    (list (latex-escape-regex-sub "o")"í")
    (list (latex-escape-regex-sub "i")"b")
    (list (latex-escape-regex-sub "r")"c")
    (list (latex-escape-regex-sub "u")"d")
    (list (latex-escape-regex-sub "v")"e")
    (list (latex-escape-regex-sub "x")"ì")
    
    
    ;; (list (latex-escape-regex "\\.\\.\\.") 'dots)
    (list (latex-escape-regex "langle")"Ë")
    (list (latex-escape-regex "rangle")"È")
    (list (latex-escape-regex "mapsto")"¶")
    (list (latex-escape-regex "to")"í")
    (list (latex-escape-regex "times")"◊")
    (list (latex-escape-regex "equiv")"a")
    (list (latex-escape-regex "star")"")
    (list (latex-escape-regex "nabla")"")
    (list (latex-escape-regex "qed")"°")
    (list (latex-escape-regex "lightning")"ﬁ")
    
    ;;New: some of my own abreviations:
    
    ;;Go to
    ;; http://www.fileformat.info/info/unicode/block/letterlike_symbols/list.htm
    ;; to find some leters, or
    ;; http://www.fileformat.info/info/unicode/block/mathematical_alphanumeric_symbols/list.htm
    ;;  My mathcal like ones are from "MATHEMATICAL BOLD SCRIPT CAPITAL", an alternative block is Letterlike symbols:
    ;;http://en.wikipedia.org/wiki/Letterlike_Symbols
    
    (list (latex-escape-regex "impl") "í")
    (list (latex-escape-regex "iff") "î")
    (list (latex-escape-regex "M") "˝")
    (list (latex-escape-regex "Mo") "˝")
    (list (latex-escape-regex "Fr") "˝")
    (list (latex-escape-regex "gt") ">")
    (list (latex-escape-regex "lt") "<")
    (list (latex-escape-regex "from") ":")
    (list (latex-escape-regex "Pow") "˝")
					;""
    (list (latex-escape-regex "La") "˝")
    
    ;;Does not work, as it pushes them all into one character
    ;; (list (latex-escape-regex "atldiamond")"ËËCÈÈ")
    ;PÂls single letter abrevs:
    (list (latex-escape-regex "L") "˝")
    (list (latex-escape-regex "N") "˝")
    (list (latex-escape-regex "E") "˝")
    (list (latex-escape-regex "C") "˝")
    (list (latex-escape-regex "D") "˝")
    
    (list (latex-escape-regex "G") "˝")
    (list (latex-escape-regex "X") "˝")
    (list (latex-escape-regex "U") "˝")
    (list (latex-escape-regex "Q") "˝")
    
    
    ;;The following are not really working perfect
    ;; (list (latex-escape-regex "overline{R}") "R")
    ;; (list (latex-escape-regex "overline{X}") "X")
    ;; (list (latex-escape-regex "overline{G}") "G")
    
    
    
    ;;The following is some ugly fucks, as it has to match brackets! This makes
    ;;$\pair[A,B]$ into $ËA,BÈ$, but can not handle nesting of the pair command,
    ;;then it does not convert the last "]" as it should. One can make one
    ;;regexp matching level of stacking, but my mind blows after even 1
    ;;level. Regular expressions can not do general, arbitrary depth,
    ;;paranthesis matching, but maybe emacs's "regexps" are expressiable enough for
    ;;this?
    (list  "\\(\\\\pair\\[\\)" "Ë")
    (list  "\\(?:\\\\pair\\[[^\]]*\\(]\\)\\)" "È")
    
    (list (latex-escape-regex "dagger") " " )
    (list (latex-escape-regex "vDash") "®" )
    (list (latex-escape-regex "bigvee") "¡" )
    (list (latex-escape-regex "bigwedge") "¿" )
    (list (latex-escape-regex "biguplus") "" )
    (list (latex-escape-regex "bigcap") "¬" )
    (list (latex-escape-regex "bigcup") "√" )
    (list (latex-escape-regex "ss") "ﬂ")
    (list (latex-escape-regex "ae") "Ê")
    (list (latex-escape-regex "oe") "S")
    (list (latex-escape-regex "o") "¯")
    (list (latex-escape-regex "AE") "∆")
    (list (latex-escape-regex "OE") "R")
    (list (latex-escape-regex "O") "ÿ")
    (list (latex-escape-regex "aa") "Â")
    (list (latex-escape-regex "AA") "≈")
    (list (latex-escape-regex "dag") " ")
    (list (latex-escape-regex "ddag") "!")
    (list (latex-escape-regex "S") "ß")
    (list (latex-escape-regex "l") "B")
    (list (latex-escape-regex "L") "A")
    (list (latex-escape-regex "copyright") "©")
    (list (latex-escape-regex "epsilon") "ı")
    (list (latex-escape-regex "phi") "’")
    (list (latex-escape-regex "vartheta") "—")
    (list (latex-escape-regex "varpi") "÷")
    (list (latex-escape-regex "varrho") "Ò")
    (list (latex-escape-regex "varsigma") "¬")
    (list (latex-escape-regex "aleph") "5")
    (list (latex-escape-regex "hbar") "")
    (list (latex-escape-regex "ell") "")
    (list (latex-escape-regex "wp") "")
    (list (latex-escape-regex "Re") "")
    (list (latex-escape-regex "Im") "")
    (list (latex-escape-regex "partial") "")
    (list (latex-escape-regex "surd") "")
    (list (latex-escape-regex "angle") " ")
    (list (latex-escape-regex "triangle") "≥")
    (list (latex-escape-regex "flat") "m")
    (list (latex-escape-regex "natural") "n")
    (list (latex-escape-regex "sharp") "o")
    (list (latex-escape-regex "clubsuit") "c")
    (list (latex-escape-regex "diamondsuit") "b")
    (list (latex-escape-regex "heartsuit") "a")
    (list (latex-escape-regex "spadesuit") "`")
    (list (latex-escape-regex "coprod") "")
    (list (latex-escape-regex "int") "+")
    (list (latex-escape-regex "prod") "")
    (list (latex-escape-regex "sum") "")
    (list (latex-escape-regex "bigotimes") "")
    (list (latex-escape-regex "bigoplus") "")
    (list (latex-escape-regex "bigodot") " ")
    (list (latex-escape-regex "oint") ".")
    (list (latex-escape-regex "bigsqcup") "")
    (list (latex-escape-regex "triangleleft") "¡")
    (list (latex-escape-regex "triangleright") "∑")
    (list (latex-escape-regex "bigtriangleup") "≥")
    (list (latex-escape-regex "bigtriangledown") "Ω")
    (list (latex-escape-regex "sqcap") "ì")
    (list (latex-escape-regex "sqcup") "î")
    (list (latex-escape-regex "uplus") "é")
    (list (latex-escape-regex "amalg") "?")
    (list (latex-escape-regex "bullet") "")
    (list (latex-escape-regex "wr") "@")
    (list (latex-escape-regex "div") "˜")
    (list (latex-escape-regex "odot") "ô")
    (list (latex-escape-regex "oslash") "ò")
    (list (latex-escape-regex "otimes") "ó")
    (list (latex-escape-regex "ominus") "ñ")
    (list (latex-escape-regex "oplus") "ï")
    (list (latex-escape-regex "mp") "")
    (list (latex-escape-regex "pm") "±")
    (list (latex-escape-regex "circ") "")
    (list (latex-escape-regex "circ") "À")
    (list (latex-escape-regex "bigcirc") "Ô")
    (list (latex-escape-regex "cdot") "≈")
    (list (latex-escape-regex "ast") "")
    (list (latex-escape-regex "star") "∆")
    (list (latex-escape-regex "propto") "")
    (list (latex-escape-regex "sqsubseteq") "ë")
    (list (latex-escape-regex "sqsupseteq") "í")
    (list (latex-escape-regex "parallel") "%")
    (list (latex-escape-regex "dashv") "£")
    (list (latex-escape-regex "vdash") "¢")
    (list (latex-escape-regex "nearrow") "ó")
    (list (latex-escape-regex "searrow") "ò")
    (list (latex-escape-regex "nwarrow") "ñ")
    (list (latex-escape-regex "swarrow") "ô")
    (list (latex-escape-regex "succ") "{")
    (list (latex-escape-regex "prec") "z")
    (list (latex-escape-regex "approx") "H")
    (list (latex-escape-regex "succeq") "}")
    (list (latex-escape-regex "preceq") "|")
    (list (latex-escape-regex "supset") "É")
    (list (latex-escape-regex "supseteq") "á")
    (list (latex-escape-regex "in") "")
    (list (latex-escape-regex "gg") "k")
    (list (latex-escape-regex "ll") "j")
    (list (latex-escape-regex "sim") "<")
    (list (latex-escape-regex "simeq") "C")
    (list (latex-escape-regex "asymp") "M")
    (list (latex-escape-regex "smile") "#")
    (list (latex-escape-regex "frown") """)
    (list (latex-escape-regex "leftharpoonup") "º")
    (list (latex-escape-regex "leftharpoondown") "Ω")
    (list (latex-escape-regex "rightharpoonup") "¿")
    (list (latex-escape-regex "rightharpoondown") "¡")
    (list (latex-escape-regex "hookrightarrow") "™")
    (list (latex-escape-regex "hookleftarrow") "©")
    (list (latex-escape-regex "bowtie") "»")
    (list (latex-escape-regex "models") "ß")
    (list (latex-escape-regex "Longrightarrow") "˘")
    (list (latex-escape-regex "longrightarrow") "ˆ")
    (list (latex-escape-regex "longleftarrow") "ı")
    (list (latex-escape-regex "Longleftarrow") "¯")
    (list (latex-escape-regex "longmapsto") "¸")
    (list (latex-escape-regex "longleftrightarrow") "˜")
    (list (latex-escape-regex "Longleftrightarrow") "˙")
    (list (latex-escape-regex "cdots") "Ô")
    (list (latex-escape-regex "vdots") "Ó")
    (list (latex-escape-regex "ddots") "Ò")
    (list (latex-escape-regex "Vert") "%")
    (list (latex-escape-regex "uparrow") "ë")
    (list (latex-escape-regex "downarrow") "ì")
    (list (latex-escape-regex "updownarrow") "ï")
    (list (latex-escape-regex "Uparrow") "—")
    (list (latex-escape-regex "Downarrow") "”")
    (list (latex-escape-regex "Updownarrow") "’")
    (list (latex-escape-regex "rceil") "	")
    (list (latex-escape-regex "lceil") "")
    (list (latex-escape-regex "rfloor") "")
    (list (latex-escape-regex "lfloor") "
")
    (list (latex-escape-regex "cong") "E")
    (list (latex-escape-regex "rightleftharpoons") "Ã")
    (list (latex-escape-regex "doteq") "P")
    )))

;;AUCTeX
(add-hook 'LaTeX-mode-hook 'latex-unicode-simplified)

;;latex-mode
(add-hook 'latex-mode-hook 'latex-unicode-simplified)
(provide 'latex-pretty-symbols)

;;; latex-pretty-symbols.el ends here
