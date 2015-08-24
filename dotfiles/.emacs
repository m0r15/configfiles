;; Highlight expression in {},[],()
(setq show-paren-style 'expression)
(show-paren-mode 2)

;; Disable GUI tools and menu
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq make-backup-files        nil) ; Don't want any backup files
(setq auto-save-list-file-name nil) ; Don't want any .saves files
(setq auto-save-default        nil) ; Don't want any auto saving

;; System-type difinition
(defun system-is-linux()
    (interactive)
    (string-equal system-type "gnu/linux"))

;; System-type difinition
(defun system-is-windows()
    (interactive)
    (string-equal system-type "windows-nt"))

;; Emacs as server
(when (system-is-linux)
    (require 'server)
    (unless (server-running-p)
        (server-start)))

;; Linux path-variable
(when (system-is-linux)
    (setq unix-init-path      "~/.emacs.d/")
    (setq unix-init-lisp-path "~/.emacs.d/lisp/")
    (setq unix-init-ct-path   "~/.emacs.d/plugins/color-theme/")
    (setq unix-init-ac-path   "~/.emacs.d/plugins/auto-complete/"))

;; Window path-variable
(when (system-is-windows)
    (setq win-init-path         "C:/.emacs.d/")
    (setq win-init-lisp-path    "C:/.emacs.d/lisp")
    (setq win-init-ct-path      "C:/.emacs.d/plugins/color-theme/")
    (setq win-init-ac-path      "C:/.emacs.d/plugins/auto-complete"))

;; Load path for plugin
(if (system-is-windows)
        (add-to-list 'load-path win-init-path)
    (add-to-list 'load-path unix-init-path))

