Long lines can be wrapped for nice visual effect in Emacs. However
the line-oriented movements are still done on logic lines, which is
not so convenient or even anti-intuitive sometimes. For example,
imagine you were on the first visible line of a wrapped very long
line(say, wrapped to totally 10 visible lines) and wanted to move to
the 5th visible line. How would you do it? It turns out that no
matter you traveled from the very beginning or the very end(you could
move-end-of-line first), it was a suffering. Long lines mode which is
included as a standard library solve most of the long lines problem;
but it only provides movements across those space-delimited
continuation lines, which is unreasonable to people whose language
does not use space characters to delimit words or even sentences, for
example, we Chinese. Visible line mode provides means to do more
general visible-line-oriented movements, irrespective of where the
lines are wrapped.

To Chinese users:
visible lines mode 支持由字符宽度不等的多种字符集组成的混合文本，可以正确处
理中英文混合编辑。
