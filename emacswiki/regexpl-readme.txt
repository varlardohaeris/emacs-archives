The function `regexpl-search-replace-list' takes a cons-ed list of
patterns and replacements.  It find the nearest match (ties defer
to list order) then replaces with the corresponding match.

For example,

(regexpl-search-replace-list '(("stopwatch" . "timer")
                               ("watch" . "wristwatch")))

will replace occurrences of "stopwatch" first with "timer", but all
other occurrences of "watch" will be replaced with "wristwatch".
