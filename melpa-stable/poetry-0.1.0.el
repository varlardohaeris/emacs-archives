;;; poetry.el --- poetry in Emacs -*- lexical-binding: t -*-

;; Copyright (C) 2019-  Gaby Launay

;; Author: Gaby Launay <gaby.launay@protonmail.com>
;; URL: https://github.com/galaunay/poetry.el
;; Package-Version: 0.1.0
;; Package-Commit: 6fb34742a66421c6104e02d65e8d391fc4494ab5
;; Version: 0.1.0
;; Keywords: Python, Tools
;; Package-Requires: ((transient "0.1.0") (cl "1.0") (xterm-color "1.8"))

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; This package offers an interface to poetry (https://poetry.eustace.io/),
;; a Python dependency management and packaging command line tool.

;;; Code:

(require 'cl-lib)
(require 'transient)
(require 'xterm-color)

;; Variables
(defconst poetry-version "0.1.0"
  "Poetry.el version.")

;; Transient interface
;;;###autoload
(define-transient-command poetry ()
  "Poetry menu."
  [:description (lambda () (format "Project: %s" (poetry-get-project-name)))
  [:if poetry-find-project-root
       :description "Dependencies"
       ("a" "Add" poetry-add)
       ("r" "Remove" poetry-remove)
       ("i" "Install" poetry-install)
       ("l" "Lock" poetry-lock)
       ("u" "Update" poetry-update)
       ("s" "Show" poetry-show)]]
  [["New project"
    ;; ("I" "Init" poetry-init)
    ("n" "New" poetry-new)]
   [:if poetry-find-project-root
    :description "Project"
        ("c" "Check" poetry-check)
        ("b" "Build" poetry-build)
        ("p" "Publish" poetry-publish)]]
  [[:if poetry-find-project-root
        :description "Shell"
        ("R" "Run a command" poetry-run)
        ("S" "Start a shell" poetry-shell)]
   ["Poetry"
    ("U" "Update" poetry-self-update)]])

;; Poetry add
(define-transient-command poetry-add ()
  "Poetry add dependency menu."
  ["Arguments"
   (poetry:--git)
   (poetry:--path)
   (poetry:--python)
   (poetry:--platform)
   ]
  ["Add"
   ("a" "Add a dependency" poetry-add-dep)
   ("d" "Add a development dependency" poetry-add-dev-dep)
   ("o" "Add an optional dependency" poetry-add-opt-dep)
   ])

(define-infix-argument poetry:--git ()
  :description "Git repository"
  :class 'transient-option
  :key "-g"
  :argument "--git=")

(define-infix-argument poetry:--path ()
  :description "Dependency path"
  :class 'transient-option
  :key "-P"
  :argument "--path=")

(define-infix-argument poetry:--python ()
  :description "Python version"
  :class 'transient-option
  :key "-p"
  :argument "--python=")

(define-infix-argument poetry:--platform ()
  :description "Platforms"
  :class 'transient-option
  :key "-t"
  :argument "--platform=")

(defun poetry-call-add (package args)
  "Add PACKAGE as a new dependency to the project.

ARGS are additionnal arguments passed to ``poetry add''."
  (let ((args (cl-concatenate 'list args
                           (transient-args 'poetry-add))))
    (poetry-call #'add nil (cl-concatenate 'list
                                       (list package)
                                       args))))

;;;###autoload
(defun poetry-add-dep (package)
  "Add PACKAGE as a new dependency to the project."
  (interactive "sPackage name: ")
  (poetry-message (format "Adding dependency: %s" package))
  (poetry-call-add package '()))

;;;###autoload
(defun poetry-add-dev-dep (package)
  "Add PACKAGE as a new development dependency to the project."
  (interactive "sPackage name: ")
  (poetry-message (format "Adding dev dependency: %s" package))
  (poetry-call-add package '("-D")))

;;;###autoload
(defun poetry-add-opt-dep (package)
  "Add PACKAGE as a new optional dependency to the project."
  (interactive "sPackage name: ")
  (poetry-message (format "Adding optional dependency: %s" package))
  (poetry-call-add package '("--optional")))

;; Poetry remove
;;;###autoload
(defun poetry-remove (package type)
  "Remove PACKAGE from the project dependencies.

TYPE is the type of dependency (dep, dev or opt)."
  (interactive (let* ((packages (cl-concatenate 'list
                                 (cl-map 'list
                                      (lambda (dep)
                                        (format "[dep]  %s" dep))
                                      (poetry-get-dependencies))
                                 (cl-map 'list
                                      (lambda (dep)
                                        (format "[dev]  %s" dep))
                                      (poetry-get-dependencies t))
                                 (cl-map 'list
                                      (lambda (dep)
                                        (format "[opt]  %s" dep))
                                      (poetry-get-dependencies nil t))))
                      (package (when packages
                                 (completing-read "Package: "
                                                  packages
                                                  nil t))))
                 (if (not package)
                     (list nil nil)
                   (string-match "^\\[\\(.*\\)\\]  \\([^[:space:]]*\\)[[:space:]]*(\\(.*\\))$" package)
                   (list (match-string 2 package)
                         (match-string 1 package)))))
  (if (not package)
      (poetry-message "No packages specified in pyproject.toml")
    (pcase type
      ("dep"
       (poetry-message (format "Removing package %s"
                               package))
       (poetry-remove-dep package))
      ("opt"
       (poetry-message (format "Removing optional package %s"
                               package))
       (poetry-remove-dep package))
      ("dev"
       (poetry-message (format "Removing development package %s"
                               package))
       (poetry-remove-dev-dep package)))))

(defun poetry-remove-dep (package)
  "Remove PACKAGE from the project dependencies."
  (poetry-call 'remove nil (list package)))

(defun poetry-remove-dev-dep (package)
  "Remove PACKAGE from the project development dependencies."
  (poetry-call 'remove nil (list package "-D")))

;; Poetry check
;;;###autoload
(defun poetry-check ()
  "Check the validity of the pyproject.toml file."
  (interactive)
  (poetry-call 'check t))

;; Poetry install
;;;###autoload
(defun poetry-install ()
  "Install the project dependencies."
  (interactive)
  (poetry-call 'install))

;; Poetry lock
;;;###autoload
(defun poetry-lock ()
  "Locks the project dependencies."
  (interactive)
  (poetry-call 'lock))

;; Poetry update
;;;###autoload
(defun poetry-update ()
  "Update dependencies as according to the pyproject.toml file."
  (interactive)
  (poetry-call 'update))

;; Poetry show
(defun poetry-show-get-packages ()
  "Return the list of package description for show."
  (poetry-call 'show)
  (with-current-buffer "*poetry*"
    (goto-char (point-min))
    (let (packs)
      (while (re-search-forward "^\\(.+\\)$" nil t)
        (push (match-string 1) packs))
      packs)))

;;;###autoload
(defun poetry-show (package)
  "Show information about package PACKAGE."
  (interactive
   (list (completing-read "Package: "
                          (poetry-show-get-packages))))
  (string-match "^\\([^[:space:]]*\\).*$" package)
  (poetry-call 'show t (list (match-string 1 package))))

;; Poetry build
;;;###autoload
(defun poetry-build ()
  "Build a package, as a tarball and a wheel by default."
  (interactive)
  (poetry-call 'build))

;; Poetry publish
;;;###autoload
(defun poetry-publish (repo username password)
  "Publish the package to a remote repository.

REPO is the repository and USERNAME and PASSWORD the
credential to use."
  (interactive (list
                (completing-read "Repository: "
                                 '("pypi"))
                (read-from-minibuffer "Username: ")
                (read-passwd "Password: ")))
  (poetry-call 'publish
               (list "-r" repo "-u" username "-p" password)))

;; Poetry new
;;;###autoload
(defun poetry-new (path)
  "Create a new Python project at PATH."
  (interactive "DProject path: ")
  (let ((default-directory path))
    (poetry-message (format "Creating new project: %s" path))
    (unless (file-directory-p path)
      (make-directory path))
    (poetry-call 'new nil (list path))))

;; Poetry run
;;;###autoload
(defun poetry-run (command)
  "Run COMMAND in the appropriate environment."
  ;; TODO: add completion with scripts from pyptoject.toml
  (interactive "sCommand: ")
  (poetry-call 'run t (split-string command "[[:space:]]+" t)))

;; Poetry shell
;;;###autoload
(defun poetry-shell ()
  "Spawn a shell within the virtual environment."
  (interactive)
  (shell "*poetry-shell*")
  (process-send-string (get-buffer-process (get-buffer "*poetry-shell*"))
                       "poetry shell\n"))

;; Poetry update
;;;###autoload
(defun poetry-self-update ()
  "Update poetry to the latest version."
  (interactive)
  (poetry-call 'self:update))

;; Helpers
(defun poetry-call (command &optional output args)
  "Call poetry COMMAND with the given ARGS.

if OUTPUT is non-nil, display the poetry buffer when fninshed."
  (let* ((prog (or (executable-find "poetry")
                   (poetry-error "Could not find 'poetry' executable")))
         (args (if (or (string= command "run")
                       (string= command "init"))
                   (cl-concatenate 'list (list (symbol-name command))
                                args)
                 (cl-concatenate 'list (list "-n" "--ansi"
                                          (symbol-name command))
                              args)))
         (poetry-buffer "*poetry*")
         error-code)
    (let ((poetry-buf (get-buffer-create poetry-buffer)))
      (with-current-buffer poetry-buf
        (delete-region (point-min) (point-max)))
      (setq error-code (apply #'call-process
                              (cl-concatenate 'list (list prog nil
                                                       (list poetry-buf t)
                                                       t)
                                           args)))
      (with-current-buffer poetry-buf
        (xterm-color-colorize-buffer)
        (goto-char (point-min))
        (while (re-search-forward "" nil t)
          (replace-match "\n" nil nil))
        (goto-char (point-min))
        (while (re-search-forward "" nil t)
          (replace-match "" nil nil))))

    (when (or output (not (= error-code 0)))
      (poetry-display-buffer))))

(defun poetry-display-buffer ()
  "Display the poetry buffer."
  (with-current-buffer "*poetry*"
    (let ((buffer-read-only nil))
      (display-buffer "*poetry*"))))

(defun poetry-get-dependencies (&optional dev opt)
  "Return the list of project dependencies.

If DEV is non-nil, install a developement dep.
If OPT is non-nil, set an optional dep."
  (with-current-file (poetry-find-pyproject-file)
                     (goto-char (point-min))
                     (if dev
                         (re-search-forward "^\\[tool\\.poetry\\.dev-dependencies\\]$")
                       (re-search-forward "^\\[tool\\.poetry\\.dependencies\\]$"))
                     (let ((beg (point))
                           (end (progn (re-search-forward "^\\[")
                                       (point)))
                           (regex (if (not opt)
                                      "^\\([^= ]*\\)[[:space:]]*=[[:space:]]*\"\\(.*\\)\""
                                    "^\\([^= ]*\\)[[:space:]]*=[[:space:]]*{version[[:space:]]*=[[:space:]]*\"\\(.*\\)\"[[:space:]]*,[[:space:]]*optional[[:space:]]*=[[:space:]]*true[[:space:]]*}$"))
                           deps)
                       (goto-char beg)
                       (while (re-search-forward regex end t)
                         (push (format "%s (%s)"
                                       (substring-no-properties (match-string 1))
                                       (substring-no-properties (match-string 2)))
                               deps))
                       (reverse deps))))

(defun poetry-find-project-root ()
  "Return the poetry project root if any."
  (locate-dominating-file default-directory "pyproject.toml"))

(defun poetry-find-pyproject-file ()
  "Return the location of the 'pyproject.toml' file."
  (let ((root (poetry-find-project-root)))
    (when root
      (concat (file-name-as-directory root) "pyproject.toml"))))

(defmacro with-current-file (file &rest body)
  "Execute the forms in BODY while temporary visiting FILE."
  `(save-current-buffer
     (let* ((file ,file)
            (keep (find-buffer-visiting file))
            (buffer (find-file-noselect file)))
       (set-buffer buffer)
       (prog1
           (progn
             ,@body)
         (when (not keep)
           (kill-buffer buffer))))))

(defun poetry-get-project-name ()
  "Return the current project name."
  (let ((file (poetry-find-pyproject-file)))
    (when file
      (with-current-file file
         (goto-char (point-min))
         (when (re-search-forward "^\\[tool\\.poetry\\]$" nil t)
           (when (re-search-forward "^name = \"\\(.*\\)\"$" nil t)
             (substring-no-properties (match-string 1))))))))

(defun poetry-message (mess)
  "Display the message MESS."
  (let ((name (poetry-get-project-name)))
    (if name
        (message "[%s] %s" name mess)
    (message "[Poetry] %s" mess))))

(defun poetry-error (mess)
  "Display the error MESS."
  (let ((name (poetry-get-project-name)))
    (if name
        (error "[%s] %s" name mess)
    (error "[Poetry] %s" mess))))


(provide 'poetry)
;;; poetry.el ends here
