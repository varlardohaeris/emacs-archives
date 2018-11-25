A simple global minor mode for working with a collection of source code files
in a directory tree, a 'project'.

projman makes it easy to switch between multiple projects and automatically
setup your emacs environment correctly for each.  A project is identified by
a root directory and a series of project keywords, variables and hook
functions that are applied when the project is opened.  Each project can also
maintain a list of open buffers, closing them when quiting a project and
automatically restoring them when re-opening it (similar to
desktop-mode).  There are also a series of commands that can be applied to
the files of a project.

Use `projman-projects' to define your projects and options (see docstring for
details).

eg.
(setq projman-projects
      '(("myproject"
         :root "c:/_proj/myproject"
         :ignore-dirs ("tests")
         :type c
         :open-hook (lambda () (global-auto-revert-mode 1))
         :compile-command "devenv.com c:/_proj/myproject/myproject.sln /build Debug"
          )))

See `projman-mode' docstring for a complete list of commands

projman-switch-project - used to switch between projects
projman-create-project - create a new project definition on the fly
                         (only allows a limited set of options to be defined)
projman-switch-buffer  - switch to another project file using `iswtichb'
projman-dired-root     - open a dired buffer on the project root
projman-grep           - run rgrep in the project root
projman-occur          - `multi-occur' against all open project buffers
projman-svn-status     - psvn status in the project root
projman-create-tags    - create a tags file for project

If `projman-use-active-file' is non-nil (the default) projman will record the
open buffers when a project is closed.  These will then be automatically
visited when the project is later opened.  If `projman-lazy-load-buffers' is
non-nil this will be done is a lazy fashion.  These project state files will
be placed in `projman-active-directory' which should already exist.  See also
`projman-close-buffers-when-close-project' to automatically close the buffers
when existing a project.

"C-cp" prefix is use for all keyboard commands.  If you want somthing else
set `projman-mode-prefix' before loading.

TIPS

You can show the current project in the frame title.  Something similar could
be used to show it in the mode line as well.
(setq frame-title-format
      '("Emacs: %b %+%+ %f"
        (projman-project-name ("  [" projman-project-name "]"))))

NOTE

Because projman makes use of the find command it will only work on a unix
type environment, but windows+cygwin works fine provided that cygwin's find
command is used instead of the windows find command. See
`projman-find-command' for a way to specify  the command path if needed.

TODO

- run tags command in background
- currently all project options are saved to the active file along with the
open buffers.  Options specified in elisp always take precedence over any in
the active file but I'm not sure this is the best thing.
- allow more project options to be set interactively
- sub-projects: allows switching project options while sharing saved state
- more extensive use of customize
