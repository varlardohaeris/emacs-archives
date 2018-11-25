defshell.el makes it easy to run different inferior shells at the
same time, by defining new commands for the Bourne (M-x sh), C (M-x
csh), Korn (M-x ksh), and Bourne Again (M-x bash) shells.  And
whereas the `M-x shell' command defined in Emacs' shell.el always
reuses the *shell* buffer, defshell.el can be customized via the
defshell-reuse-buffer and defshell-rename-buffer-uniquely options
(e.g. for `M-x sh') to reuse the *sh* buffer or create a *sh*<2> (or
*sh*<N>) buffer.

defshell.el also provides the `defshell' macro to define additional
shell commands.  E.g. to define the Foo shell (M-x fsh):
	(defshell "/bin/fsh" "Foo")
or to define a DNS lookup shell (M-x nslookup):
	(defshell "/usr/sbin/nslookup" "DNS lookup")
and since `nslookup` doesn't accept the -i option:
	(setq explicit-nslookup-args '())
