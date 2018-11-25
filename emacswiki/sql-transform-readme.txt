This code provides three things:

1. INSERT, SELECT, UPDATE, and DELETE statements can be converted
   into each other:  Place point within a SELECT statement and
   type M-x sql-to-update RET in order to transform the SELECT
   statement into an UPDATE statement.

2. SQL statement pretty printer:  Place point within a SELECT
   statement and type M-x sql-to-select RET in order to rewrite
   the SELECT statement.

This is usefull if you are developping code that needs a set of
INSERT, SELECT, UPDATE, and DELETE statements for the same table.
This set of functions will parse any of these four SQL statements and
rewrite them for you (if they are simple, eg. no joins to other
tables).  Just mark a SQL statement and call sql-to-insert,
sql-to-select, sql-to-update, or sql-to-delete.

Obviously this works best if you start with a SELECT or UPDATE
statement, because DELETE statements don't have columns and
bindvariables, and INSERT statements don't have where clauses.

A possible way to customize your .emacs would be to use the following
(assuming you are using sql-mode.el from the page mentioned above):

(add-hook 'sql-mode-hook
	  (function (lambda ()
		      (local-set-key "\C-cu" 'sql-to-update))))
