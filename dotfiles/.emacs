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
    (setq unix-init-ac-path   "~/.emacs.d/plugins/auto-complete/")
    (setq unix-init-ac-dict-path "~/.emacs.d/plugins/auto-complete/dict/"))

;; Window path-variable
(when (system-is-windows)
    (setq win-init-path         "C:/.emacs.d/")
    (setq win-init-lisp-path    "C:/.emacs.d/lisp")
    (setq win-init-ct-path      "C:/.emacs.d/plugins/color-theme/")
    (setq win-init-ac-path      "C:/.emacs.d/plugins/auto-complete"))

;; Load path for plugin
(if (system-is-windows)
    (progn
        (add-to-list 'load-path win-init-path)
        (add-to-list 'load-path win-init-lisp-path))
    (progn
        (add-to-list 'load-path unix-init-path)
        (add-to-list 'load-path unix-init-lisp-path)))

;; Indent settings
(setq-default indent-tabs-mode nil) ; off tab indent
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)
(global-set-key (kbd "RET") 'newline-and-indent) 
(setq lisp-indent-function 'common-lisp-indent-function)

;; Line number activate
(require 'linum+)
(setq linum-format " %d")
(global-linum-mode 1)

;; IDO plugin
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-virtual-buffers      t)
(setq ido-enable-flex-matching t)

;; Buffer Selection and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ; list of buffers C-x C-b
(setq bs-configurations '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(global-set-key (kbd "<f2>") 'bs-show)

;; Popup plugin
(require 'popup)

;; Auto-complete plugin <http://www.emacswiki.org/emacs/AutoComplete>
(if (system-is-windows)
    (add-to-list 'load-path win-init-ac-path)
    (add-to-list 'load-path unix-init-ac-path))    
(require 'auto-complete-config)
(ac-config-default)
(if (system-is-windows)
    (add-to-list 'ac-dictionary-directories win-init-ac-dict-path)
    (add-to-list 'ac-dictionary-directories unix-init-ac-dict-path))
(setq ac-auto-start t)
(setq ac-auto-show-menu t)
(global-auto-complete-mode t)
