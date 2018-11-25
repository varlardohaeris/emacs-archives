Installation:

To use this package, put the following line in your .emacs:

   (require 'sierotki)

If you do not want to load this package before it is necessary, you
can make use of the `autoload' feature, e.g. adding to your .emacs
the following lines

   (autoload 'tex-magic-space-mode "sierotki"
             "TeX Magic Space minor mode" t)
   (define-key mode-specific-map " " 'tex-magic-space-mode)

Then after turning on `tex-magic-space-mode' via `C-c SPC'
the whole package will be loaded.  Attention: using autoload means
that this mode _can't_ be turned on automatically in LaTeX modes.

If you want to have the TeX Magic Space mode turned on in known
TeX modes put the following line in your .emacs after (require 'sierotki)

   (turn-on-tex-magic-space-in-tex-modes)

If you want filling (`fill-paragraph' and `auto-fill-mode') to not break
line after single letter words, put one of the following lines in your
.emacs after (require 'sierotki)

   (setq fill-nobreak-predicate 'fill-single-letter-word-nobreak-p)
or
   (setq fill-nobreak-predicate 'fill-tex-magic-space-nobreak-p)


Installation[pl]:

Aby uĹźyÄ tego pakietu, umieĹÄ nastÄpujÄcÄ linijkÄ w swoim pliku .emacs

   (require 'sierotki)

JeĹli nie chcesz go ĹadowaÄ zanim nie bÄdzie potrzebny, moĹźesz uĹźyÄ
Ĺadowania na ĹźÄdanie, np. dodajÄc do .emacs nastÄpujÄce linie

   (autoload 'tex-magic-space-mode "sierotki"
             "TeX Magic Space minor mode" t)
   (define-key mode-specific-map " " 'tex-magic-space-mode)

WĂłwczas po wciĹniÄciu `C-c SPC' zostanie wĹÄczony TeX Magic Space mode
i zostanie zaĹadowana reszta funkcji.  Uwaga: przy uĹźywaniu
automatycznego Ĺadowania ten tryb _nie moĹźe_ byÄ automatycznie wĹÄczany
w trybach LaTeX-owych.

JeĹli chcesz by TeX Magic Space mode byĹ automatycznie wĹÄczany
w znanych trybach TeX-owych dodaj nastÄpujÄcÄ linijkÄ do swojego pliku
.emacs po (require 'sierotki)

   (turn-on-tex-magic-space-in-tex-modes)

JeĹli chcesz by Emacs ĹamiÄc wiersze (`fill-paragraph' i
`auto-fill-mode') nie ĹamaĹ linii za jednoliterowymi wyrazami
('sierotkami'), dodaj jednÄ z poniĹźszych linii do swojego .emacs
za (require 'sierotki)

   (setq fill-nobreak-predicate 'fill-single-letter-word-nobreak-p)
lub
   (setq fill-nobreak-predicate 'fill-tex-magic-space-nobreak-p)



Description:

The purpose of this package is to connect some defined words (by default
one letter Polish prepositions) with the following words by tilde, which
is the non-breakable space in TeX.  This is needed to avoid one letter
prepositions at line endings in TeX documents, which is required by
the Polish and Czech typography/typesetting rules.

This program serves two purposes.  First of them is to check the text
and suggest adding missing tildes in some places.  This function is
implemented in `tex-hard-spaces' via `query-replace-regexp'.  It is
provided for convenience only to have both functionalities in the
same module.  More elaborated implementation can be found in the
`tildify' package which is part of GNU Emacs (ATTENTION: default
variable settings in the tildify package are suited for Czech
language, those here are for Polish).

The second purpose is the automatic, in-the-fly insertion of tildes
after one letter prepositions during writing.  It is implemented
via the `tex-magic-space' command which is a kind of electric space
and should be bound to SPC to work.  To activate this functionality
you have to turn on `tex-magic-space-mode'.  After loading this
package this command is bound to the `C-c SPC'.  The minor mode TeX
Magic Space can be also turned on from the modeline minor mode
menu.  This mode is denoted by " ~" in the modeline.  The ":Chk"
after " ~" in the modeline shows that test are enabled.  You can
enable tests using `tex-magic-space-toggle-checking' command, bound to the
`C-c C-SPC'.

For the time being the tests in `tex-magic-space-tests' are in early beta
phase; if you want to insert ` ' where `tex-magic-space-mode' inserts
`~', use `C-q SPC' to enter single space, or turn off the TeX Magic Space
mode fro editing the fragment of document where nonbreakable spaces are
not needed.

The TeX Magic Space mode can be automatically turned on in the TeX modes
by adding the equivalent of `turn-on-tex-magic-space-mode' to the
hooks defined in the variable `tex-magic-space-mode-hooks-list' using
the command `turn-on-tex-magic-space-in-tex-modes'.

NEW: There are also defined two fill predicates,
`fill-single-letter-word-nobreak-p' and `fill-tex-magic-space-nobreak-p',
which after set as value of `fill-nobreak-predicate' variable makes
filling (`M-q' aka `fill-paragraph' and `auto-fill-mode') to not break
line after single letter words.  The latter predicate uses the same test
as TeX Magic Space mode.  Not shown in modeline.

See also: http://www.emacswiki.org/cgi-bin/wiki/NonbreakableSpace
Documentation and comments: Jakub NarÄbski.


Description[pl]:

Ten pakiet sĹuĹźy do dowiÄzywania zdefiniowanych wyrazĂłw (domyĹlnie
jednoliterowych spĂłjnikĂłw) do nastÄpujÄcych po nich sĹĂłw za pomocÄ znaku
`~' (tyldy), nieĹamliwej spacji TeX-owej.  SĹuĹźy to temu, aby w
dokumentach TeX-owych uniknÄÄ jednoliterowych spĂłjnikĂłw na koĹcach linii,
co jest wymagane przez polskie (i czeskie) reguĹy typograficzne.

Pakiet ten dostarcza dwu funkcjonalnoĹci.  PierwszÄ z nich jest
sprawdzenie (istniejÄcego) tekstu i zasugerowanie dodania brakujÄcych
tyld.  Jest ona implementowana przez komendÄ `tex-hard-spaces', za pomocÄ
`query-replace-regexp'.  TÄ samÄ (a nawet rozszerzonÄ) funkcjonalnoĹÄ
znaleĹşÄ moĹźna w pakiecie `tildify' (UWAGA: domyĹlne ustawienia w tym
pakiecie sÄ dostosowane do jÄzyka czeskiego).

DrugÄ z funkcjonalnoĹci jest automatyczne wpisywanie tyld po
jednoliterowych spĂłjnikach podczas pisania tekstu (w locie).  Jest ona
implementowana przez komendÄ `tex-magic-space', ktĂłrÄ naleĹźy podpiÄÄ do
spacji.  Do aktywowania tej funkcjonalnoĹci naleĹźy wĹÄczyÄ
`tex-magic-space-mode'.  Po zaĹadowaniu tego pakietu polecenie to jest
przypisane do `C-c SPC'.  Tryb (minor mode) TeX Magic Space moĹźna
aktualnie wĹÄczyÄ takĹźe z modeline minor mode menu; jest on oznaczany za
pomocÄ " ~".  Dodatkowe oznaczenie ":Chk" po " ~" informuje, Ĺźe
porady/testy sÄ aktywne.  Testy moĹźna wĹÄczyÄ za pomocÄ polecenia
`tex-magic-space-toggle-checking' przypisanego do `C-c C-SPC'.

Na razie sprawdzanie czy naleĹźy wstawiaÄ nieĹamliwe spacje po
jednoliterowych spĂłjnikach jest w wersji wstÄpnej; jeĹli chcesz wstawiÄ
` ' tam gdzie mode wstawia `~' uĹźyj `C-q SPC' lub wyĹÄcz tryb na czas
edycji fragmentu gdzie nieĹamliwe spacje nie sÄ poĹźÄdane.

FunkcjonalnoĹÄ ta moĹźe byÄ automatycznie wĹÄczana w trybach TeX-owych za
pomocÄ dodania odpowiednika `turn-on-tex-magic-space-mode' do odpowiednich
haczykĂłw (zdefiniowanych w zmiennej `tex-magic-space-mode-hooks-list') za
pomocÄ polecenia (funkcji) `turn-on-tex-magic-space-in-tex-modes'.

NOWE: ZostaĹy takĹźe zdefiniowane dwie funkcje
`fill-single-letter-word-nobreak-p' i `fill-tex-magic-space-nobreak-p',
ktĂłre wstawione jako wartoĹÄ zmiennej `fill-nobreak-predicate' powodujÄ,
Ĺźe Emacs ĹamiÄc linie (`M-q' czyli `fill-paragraph', oraz
`auto-fill-mode') nie zostawia samotnych jednoliterowych wyrazĂłw na koĹcu
wiersza (sierotek).  Druga z funkcji uĹźywa tego samego testu co  TeX
Magic Space mode.  Nie pokazywane automatycznie w modeline.

Zobacz takĹźe: http://www.emacswiki.org/cgi-bin/wiki/NonbreakableSpace
Dokumentacja i komentarze: Jakub NarÄbski.
