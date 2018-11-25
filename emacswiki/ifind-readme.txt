The `ifind' command is like `igrep-find' (which in turn is like
`grep-find'), except that it uses the awk program to show just one
line of all files whose names match the user-specified regex.

A default file name pattern is provided (based on the current
buffer), and the user can customize the `ifind-awk-regex' variable to
determine which line of each matching file is shown (to provide a
peek at its content).
