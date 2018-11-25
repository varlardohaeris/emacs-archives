 This is an extension to display disk usage in the mode line.  Disk
 space remaining is updated every `df-interval' seconds.

 If you work with a lot of users sharing the same partition, it
 sometimes happens that you have no place left to save your work,
 which can be extremely annoying, and might lead to loss of work.
 This package allows you to have disk space available and buffer
 size displayed in the mode line, so you know when you can save
 your file or when it is time to do some tidying up.

 Comments and suggestions are welcome.

 df is simple to use:
 - Make sure this file is in your load-path
 - Put in ~/.emacs:
     (autoload 'df-mode "df-mode" nil t)
     (df-mode 1)
   The minor mode can be toggled with M-x df-mode.

 Version: 2004-11-05
 Note: this is a rewrite of df.el by Benjamin Drieu:
 - Added customization
 - Completely rewrote internals (the actual "engine" has been
   simplified, but the customisation enhanced)
 - The partitions that are checked are now buffer-local
 - Added the function that checks disk space before writing a
   buffer

 2005-05-09
 - Added test for the case that `buffer-file-name' is a
   non-existing file (e.g. because newly created)
 - Use define-minor-mode, and remove the code that was made
   redundant by this.  (df-mode var and function,
   df-mode-set-df-mode).  The trigger for the mode is now solely
   df-mode, and not df-timer anymore.  df-update now first checks
   df-mode.
 - Use new hook variable names in df-mode.
 - Use a cancel and initialisation function for the timer (to avoid
   having more than one timer, eg after starting from a desktop)
 - Add check the df-interval > 0.  If not, the df check is not done
   anymore, but the mode is left on.
