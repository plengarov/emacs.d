;;; cmake-projectile.el --- CMake integration with Projectile

;; Copyright (C) 2017 Dragomir Todorov

;; This file is NOT part of GNU Emacs.

;;; Code:

(require 'projectile)

(defvar projectile-cmake-cmd-map
  (make-hash-table :test 'equal)
  "A mapping between projects and the last run command used on them.")

(defvar projectile-project-cmake-cmd nil
  "The command to use with `projectile-run-project'.
It takes precedence over the default command for the project type when set.
Should be set via .dir-locals.el.")

(defun projectile-default-cmake-command (project-type)
  "Retrieve default cmake command for PROJECT-TYPE."
  (plist-get (gethash project-type projectile-project-types) 'cmake-command))

(defun projectile-cmake-command (project)
  "Retrieve the cmake command for PROJECT."
  (or (gethash project projectile-cmake-cmd-map)
      projectile-project-cmake-cmd
      (projectile-default-cmake-command (projectile-project-type))))

;;;###autoload
(defun projectile-cmake-project (arg)
  "Run project cmake command.

Normally you'll be prompted for a cmake command, unless
variable `cmake-read-command'.  You can force the prompt
with a prefix ARG."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (default-cmd (projectile-cmake-command project-root))
         (cmake-cmd (projectile-maybe-read-command arg default-cmd "CMake command: "))
         (default-directory project-root))
    (puthash project-root cmake-cmd projectile-cmake-cmd-map)
    (projectile-run-compilation cmake-cmd)))

(global-set-key (kbd "C-c p C") #'projectile-cmake-project)

(provide 'cmake-projectile)
