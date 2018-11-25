;;; emacs-droid.el --- Android application development tools for Emacs

;; emacs-droid inspired by remvee's android-mode: https://github.com/remvee/android-mode

;; Copyright (C) 2013 Daehyub Kim
;; Copyright (C) 2009-2012 R.W van 't Veer

;; Author: Daehyub Kim <lateau@gmail.com>
;; Created: 1 Jan 2013
;; Keywords: android
;; Version: 0.0.0
;; URL: https://github.com/lateau/emacs-droid

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'cl)

(defgroup emacs-droid nil
  ""
  :prefix "emacs-droid-"
  :group 'applications)

(defcustom droid-sdk-root-dir nil
  "Android sdk root path."
  :type 'string
  :group 'emacs-droid)

(defcustom droid-ndk-root-dir nil
  "Android ndk root path."
  :type 'string
  :group 'emacs-droid)

(defconst droid-manifest "AndroidManifest.xml"
  "AndroidManifest.xml")

(defconst droid-local-properties "local.properties"
  "Android local.properties file name for finding project root and
getting each projects specified android sdk.")

(defconst droid-prop-sdk-dir "sdk\\.dir"
  "Root path of Android SDK that is related to current project.")

(defconst droid-external-commands '(sdk-manager "tools/android"
                                    avd-manager "tools/android"
                                    ddms "tools/monitor"
                                    monitor "tools/monitor"
                                    adb "platform-tools/adb"
                                    ndk-build "ndk-build"
                                    ant "ant"
                                    emulator "tools/emulator")
  "Android SDK or external commands")

(defface droid-logcat-other-face '((t (:underline nil)))
  ""
  :group 'emacs-droid)

(defface droid-logcat-debug-face '((t (:foreground "LawnGreen" :underline nil)))
  ""
  :group 'emacs-droid)

(defface droid-logcat-info-face '((t (:foreground "WhiteSmoke" :underline nil)))
  ""
  :group 'emacs-droid)

(defface droid-logcat-verbose-face '((t (:foreground "DeepSkyBlue" :underline nil)))
  ""
  :group 'emacs-droid)

(defface droid-logcat-warning-face '((t (:foreground "OrangeRed" :underline nil)))
  ""
  :group 'emacs-droid)

(defface droid-logcat-error-face '((t (:foreground "red" :bold t :underline nil)))
  ""
  :group 'droid-mode)

(defun droid-find-sdk-root (type)
  "Return android sdk root path or find it in user's home directory."
  (if (eval (intern (concat "droid-" type "-root-dir")))
      (eval (intern (concat "droid-" type "-root-dir")))
    (let ((sdks (remove nil
                        (map 'list
                             '(lambda (x)
                                (and (string-match (concat "^android-" type "-.+") x) x))
                             (directory-files "~/")))))
      (if sdks
          (concat "~/" (completing-read "Select SDK: " sdks))
        (error "Android sdk couldn't be found.")))))

(defun droid-find-project-root ()
  "Find and return current Android project root path.
emacs-droid assumes that Android project root path should has local.properties."
  (or (locate-dominating-file default-directory droid-local-properties)
      (locate-dominating-file default-directory droid-manifest)))

(defmacro in-droid-project (body &optional error-body)
  "Eval body in current Android project root directory."
  `(let ((default-directory (droid-find-project-root)))
     (if default-directory
         ,body
       ,error-body)))

(defun droid-get-property (key)
  "Retrieve a value related to the key from Android properties."
  (in-droid-project
   (with-temp-buffer
     (insert-file-contents droid-local-properties)
     (string-match (format "^sdk.*" key) (buffer-string))
     (second (split-string (match-string-no-properties 0 (buffer-string)) "=")))))

(defun droid-get-bufname (name)
  ""
  (format "*droid-%s*" name))

;; TODO: #1 error handling for external commands
(defun droid-run-external-command (key bufname &optional argv)
  (let ((sdk-dir (droid-get-property droid-prop-sdk-dir))
        (command (getf droid-external-commands key))
        (name (droid-get-bufname bufname)))
    (if (process-status name)
        (message (concat command " is already running"))
      (start-process-shell-command name name (concat sdk-dir "/" command) argv)
      (message (concat command " is running in " (format "%d" (process-id (get-process name))))))))

(defun droid-list-targets (command)
  ""
  (with-temp-buffer
    (let ((result nil)
          (offset 0))
      (insert-string (shell-command-to-string (concat command " list target")))
      (while (string-match "id: [[:digit:]]+ or \"\\(.*\\)\"" (buffer-string) offset)
        (setq result (cons (match-string 1 (buffer-string)) result))
        (setq offset (match-end 0)))
      (if result
          result
        (yes-or-no-p "No android target be found. Install now? ")))))

(defun droid-create-project (name package activity)
  "Create a new Android project in current directory."
  (interactive "sEnter project name: \nsEnter package name: \nsEnter default activity: ")
  (and (string= "" name) (error "Project name required."))
  (and (string= "" package) (error "Package name required."))
  (and (string= "" activity) (error "Default activity required."))
  (let* ((sdk-dir (droid-find-sdk-root "sdk"))
         (command (getf droid-external-commands 'sdk-manager))
         (type (completing-read "Select project type: " '("project" "lib-project" "test-project")))
         (target (completing-read "Select target: " (droid-list-targets (concat sdk-dir "/" command))))
         (argv (concat " create " type " -p " name " -a " activity " -t " target " -k " package)))
    (shell-command (concat sdk-dir "/" command argv))))

(defun droid-update-project (name)
  "Update current Android project."
  (interactive "sEnter a new project name. Enter blank to skip this step: ")
  (in-droid-project
   (when (file-exists-p droid-manifest)
     (let* ((sdk-dir (droid-find-sdk-root "sdk"))
            (command (getf droid-external-commands 'sdk-manager))
            (type (completing-read "Select project type: " '("project" "lib-project" "test-project")))
            (target nil)
            (subprojects-p nil)
            (argv (concat " update " type " ")))
       (cond ((string= "project" type)
              (setq argv (concat argv " -p . "))
              (setq target (completing-read "Select target. Enter blank to skip this step: " (droid-list-targets (concat sdk-dir "/" command))))
              (setq subprojects-p (yes-or-no-p "Update any projects in sub-folders. "))
              (and (not (string= "" name))
                   (setq argv (concat argv " -n " name)))
              (and (not (string= "" target))
                   (setq argv (concat argv " -t " target)))
              (and subprojects-p
                   (setq argv (concat argv " -s "))))
             ((string= "lib-project" type)
              (setq target (completing-read "Select target. Enter blank to skip this step: " (droid-list-targets (concat sdk-dir "/" command))))
              (setq argv (concat argv " -p ."))
              (and (not (string= "" target))
                   (setq argv (concat argv " -t " target))))
             ((string= "test-project" type)
              (setq argv (concat argv " -m " (completing-read "Select app directory: " (remove nil (map 'list '(lambda (x) (and (file-directory-p x) x)) (directory-files "."))))))
              (setq argv (concat argv " -p . ")))
             (t (error "type should not be blank.")))
       (shell-command (concat sdk-dir "/" command argv))))))

(defun droid-open-manifest ()
  "Open AndroidManifest.xml on a current Android project."
  (interactive)
  (in-droid-project
   (when (file-exists-p droid-manifest)
     (find-file droid-manifest))))

(defmacro droid-defun-launch-external (key &optional argv)
  `(defun ,(intern (format "droid-launch-%s" (symbol-name key)))
     ()
     (interactive)
     (droid-run-external-command ',key ,(symbol-name key) ,argv)))

(droid-defun-launch-external sdk-manager "sdk")
(droid-defun-launch-external avd-manager "avd")
(droid-defun-launch-external ddms)
(droid-defun-launch-external monitor)

(defun droid-run-ant-task (task)
  "Run an android ant task."
  (in-droid-project
   (let ((ant (getf droid-external-commands 'ant)))
      (compile (concat ant " -e " task)))))

(defmacro droid-defun-ant-task (task)
  `(defun ,(intern (format "droid-ant-%s" (symbol-name task)))
     ()
     (interactive)
     (droid-run-ant-task ,(symbol-name task))))

(droid-defun-ant-task debug)
(droid-defun-ant-task clean)

(defun droid-ant-installd ()
  "Install debug apk package to an attached device."
  (interactive)
  (with-droid-device
   (let ()
     (setenv "ANDROID_SERIAL" device)
     (droid-run-ant-task "installd"))))

(defun droid-ant-uninstall ()
  "Uninstall debug apk package from an attached device."
  (interactive)
  (with-droid-device
   (let ()
     (setenv "ANDROID_SERIAL" device)
     (droid-run-ant-task "uninstall"))))

(defun droid-get-devices ()
  "Retrieve attached Android device list or nil."
  (let* ((sdk-dir (droid-get-property droid-prop-sdk-dir))
         (command (concat sdk-dir "/" (getf droid-external-commands 'adb) " devices"))
         (output (shell-command-to-string command))
         (devices (remove "" (cdr (split-string output "\n")))))
    (map 'list #'(lambda (x)
                   (car (split-string x "\t"))) devices)))

(defun droid-get-avds ()
  "Return any usable avd list or nil."
  (let* ((sdk-dir (droid-get-property droid-prop-sdk-dir))
         (command (concat sdk-dir "/" (getf droid-external-commands 'sdk-manager) " list avd"))
         (output (shell-command-to-string command))
         (offset 0)
         (devices nil))
    (while (string-match "Name:\s\\(.*\\)" output offset)
      (setq devices (add-to-list 'devices (match-string 1 output)))
      (setq offset (match-end 0)))
    (and devices (reverse devices))))

(defmacro with-droid-device (&rest body)
  "Run command with an Android device."
  `(let* ((devices (droid-get-devices))
          (device (or (and (> (length devices) 1) (completing-read "Select device: " (droid-get-devices)))
                      (car devices))))
     (if device
         ,@body
       (error "No Android device attached."))))

(defun droid-launch-avd ()
  "Launch AVD."
  (interactive)
  (let* ((devices (droid-get-avds))
         (device (completing-read "Select AVD: " devices))
         (name (format "avd-%s" device))
         (argv (format "-avd %s" device)))
    (if (member device devices)
        (droid-run-external-command 'emulator name argv)
      (error (format "Invalid device: %s" device)))))

(defun droid-shell ()
  "Launch adb shell in comint mode."
  (interactive)
  (with-droid-device
   (let* ((name (format "droid-shell-%s" device))
          (sdk-dir (droid-get-property 'sdk-dir))
          (adb (getf droid-external-commands 'adb))
          (command (concat sdk-dir "/" adb))
          (argv `("-s" ,device "shell")))
     (apply 'make-comint name command nil argv)
     (delete-other-windows)
     (split-window)
     (other-window -1)
     (switch-to-buffer (format "*%s*" name)))))

(defvar droid-logcat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") 'next-logical-line)
    (define-key map (kbd "p") 'previous-logical-line)
    (define-key map (kbd "q") 'delete-window)
    map))

(defvar droid-logcat-compilation-regexp
  '(("^\\(D/.+\\)(.+[0-9]+):\s\\(.+\\)$" 2 nil nil 0 nil (2 'droid-logcat-debug-face))
    ("^\\(I/.+\\)(.+[0-9]+):\s\\(.+\\)$" 2 nil nil 0 nil (2 'droid-logcat-info-face))
    ("^\\(V/.+\\)(.+[0-9]+):\s\\(.+\\)$" 2 nil nil 0 nil (2 'droid-logcat-verbose-face))
    ("^\\(W/.+\\)(.+[0-9]+):\s\\(.+\\)$" 2 nil nil 1 nil (2 'droid-logcat-warning-face))
    ("^\\(E/.+\\)(.+[0-9]+):\s\\(.+\\)$" 2 nil nil 2 nil (2 'droid-logcat-error-face))))

(defun droid-logcat-filter (proc string)
  ""
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((moving (= (point) (process-mark proc)))
            (buffer-read-only nil)
            (string (replace-regexp-in-string "(  " "(" (replace-regexp-in-string "\t" "  " (replace-regexp-in-string "" "" string)))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (and moving (goto-char (process-mark proc)))))))

;; TODO: #6 add a find file stuff
(defun droid-logcat-find-file-or-browse-url (n &optional reset)
  "This modfied next-error-internal provides web search stuff using browse-url
with compilation-next-error--message as a keyword. This is pretty good enough to
know about a next-error, although it's implement is dirty and not in compilation-minor-mode's general way."
  (let ((logcat-next-error-message nil)
        (logcat-search-query nil ))
    (setq logcat-next-error-message
          (caar (elt (elt (substring
                           (compilation-next-error (or 0 1) nil
                                                   (or compilation-current-error
                                                       compilation-messages-start
                                                       (point-min))) 1 2) 0) 2)))
    (setq logcat-search-query
          (read-from-minibuffer "Web Search Keywords: " logcat-next-error-message))
    (if (= (length logcat-search-query) 0)
        (message "Quit")
      (browse-url (format android-mode-search-query-format logcat-search-query)))))

(defvar droid-logcat-buffer nil)

(defun droid-logcat ()
  ""
  (interactive)
  (with-droid-device
   (let* ((name (format "logcat-%s" device))
          (bufname (droid-get-bufname name))
          (argv (format "-s %s logcat" device)))
     (setq droid-logcat-buffer bufname)
     (droid-run-external-command 'adb name argv)
     (set-process-filter (get-buffer-process bufname) 'droid-logcat-filter)
     (with-current-buffer bufname
       (setq buffer-read-only t)
       (use-local-map droid-logcat-map)
       (compilation-minor-mode)
       (set (make-local-variable 'next-error-function) 'droid-logcat-find-file-or-browse-url)
       (set (make-local-variable 'compilation-error-regexp-alist) droid-logcat-compilation-regexp)
       (font-lock-mode t))
     (delete-other-windows)
     (split-window)
     (other-window -1)
     (switch-to-buffer (droid-get-bufname name))
     (goto-char (point-max)))))

(defun droid-ndk-build ()
  "Run ndk-build."
  (interactive)
  (in-droid-project
   (let ((ndk-dir (droid-find-sdk-root "ndk"))
         (ndk-build (getf droid-external-commands 'ndk-build)))
     (compile (concat ndk-dir "/" ndk-build)))
   (error "Project root couldn't be found. Please update your project with droid-update-project.")))

(defun droid-ndk-build-clean ()
  "Rune ndk-build clean."
  (interactive)
  (in-droid-project
   (let ((ndk-dir (droid-find-sdk-root "ndk"))
         (ndk-build (getf droid-external-commands 'ndk-build)))
     (compile (concat ndk-dir "/" ndk-build " clean")))
   (error "Project root couldn't be found. Please update your project with droid-update-project.")))

;;;###autoload

(provide 'emacs-droid)

;;; emacs-droid.el ends here
