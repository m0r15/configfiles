;;;;
;; GNU Emacs configuration file
;;
;; Author: m0r15
;; Contact: serenkovav@gmail.com
;;;;

;;; System cheking
(defun system-is-linux()
    "Linux system checking"
    (interactive)
    (string-equal system-type "gnu/linux"))

(defun system-is-windows()
    "Windows system checking"
    (interactive)
    (string-equal system-type "windows-nt"))


;;; Start Emacs server
(when (system-is-linux)
    (require 'server)
    (unless (server-running-p)
        (server-start)))


;;; User and email
(setq user-full-name    "m0r15")
(setq user-mail-address "serenkovav@gmail.com")


;;; Package manager
;; Initial package and add Melpa repo
; TODO: Реализовать подключение к Melpa



;; Highlight expression in {},[],()
(setq show-paren-style 'expression)
(show-paren-mode 2)

;; Disable GUI tools and menu
(tooltip-mode                 -1)
(menu-bar-mode                -1)
(tool-bar-mode                -1)
(scroll-bar-mode              -1)
(blink-cursor-mode            -1)
(setq use-dialog-box         nil)
(setq redisplay-dont-pause     t) ;; more speed for drawing buff
(setq ring-bell-function 'ignore)

(setq make-backup-files        nil) ; Don't want any backup files
(setq auto-save-list-file-name nil) ; Don't want any .saves files
(setq auto-save-default        nil) ; Don't want any auto saving

;; Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; Inhibit startup/splash screen
(setq inhibit-splash-screen t)
(setq ingibit-startup-message t)

;; Electric-mode settings
(electric-pair-mode 1) ;; autoclose {},[],()
(electric-indent-mode -1)

;; Delete selection
(delete-selection-mode t)

;; Fringe settings
(fringe-mode '(8 . 0)) ;; limit only left
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Display file size/date in mode-line
(setq display-time-24hr-format t)
(display-time-mode             t) ;; show clock in mode-line
(size-indication-mode          t) ;; size of file in %

;; Line wrapping
(setq word-wrap          t)
(global-visual-line-mode t)



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
    (setq win-init-ac-path      "C:/.emacs.d/plugins/auto-complete")
    (setq win-init-ac-dict-path "C:/.emacs.d/plugins/auto-complete/dict"))

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
(setq linum-format " %d ")
(global-linum-mode 1)

;; IDO plugin
(require 'ido)
(ido-mode                      t)
(icomplete-mode                t)
(ido-everywhere                t)
(setq ido-virtual-buffers      t)
(setq ido-enable-flex-matching t)

;; Imenu
(require 'imenu)
(setq imenu-auto-rescan t) ;; auto reload list in buffer
(setq imenu-use-popup-menu nil) ;; dialogs Imenu only in minibuffer
(global-set-key (kbd "<f6>") 'imenu) ;; press F6

;; Buffer Selection and ibuffer settings
(require 'bs)
(require 'ibuffer)
(defalias 'list-buffers 'ibuffer) ; list of buffers C-x C-b
(setq bs-configurations '(("files" "^\\*scratch\\*" nil nil bs-visits-non-file bs-sort-buffer-interns-are-last)))
(global-set-key (kbd "<f2>") 'bs-show)

;; Popup plugin
(require 'popup)

;; Org-mode settings
(require 'org)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)
(add-to-list 'auto-mode-alist '("\\.org?" . Org-mode)) ;; assoc with *.org

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
(add-to-list 'ac-sources 'ac-source-variables)
(add-to-list 'ac-sources 'ac-source-functions)
(add-to-list 'ac-sources 'ac-source-dictionary)
(add-to-list 'ac-sources 'ac-source-words-in-all-buffer) ;; search in all buffers
(add-to-list 'ac-sources 'ac-source-files-in-current-dir)

;; Color-theme difinition <http://www.emacswiki.org/emacs/ColorTheme>
;; choose your theme M-x color-theme-<TAB> RET
(defun color-theme-init()
    (require 'color-theme)
;;    (color-theme-initialize)
    (setq color-theme-is-global t)
    (color-theme-charcoal-black))
(if (system-is-windows)
    (when (file-directory-p win-init-ct-path)
        (add-to-list 'load-path win-init-ct-path)
        (color-theme-init))
    (when (file-directory-p unix-init-path)
        (add-to-list 'load-path unix-init-ct-path)
        (color-theme-init)))

;; Sr-speedbar http://www.emacswiki.org/emacs/SrSpeedbar
(require 'sr-speedbar)
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)

;; Mark-down http://emacswiki.org/emacs/MarkdownMode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md?" . markdown-mode))



;; Key bindings:
;; Stop using arrow
(global-unset-key [up])
(global-unset-key [down])
(global-unset-key [left])
(global-unset-key [right])

;; Motion keys
;; Like vim keys motion
;; Go to previous line (k - up)
(global-set-key (kbd "M-k") 'previous-line)
;; Go to the next line (j - down)
(global-set-key (kbd "M-j") 'next-line)
;; Backward char (h - left)
(global-set-key (kbd "M-h") 'backward-char)
;; Forward char (l - right)
(global-set-key (kbd "M-l") 'forward-char)
;;NOT vim keys
;; Backward word
(global-set-key (kbd "M-u") 'backward-word) 
;; Forward word
(global-set-key (kbd "M-o") 'forward-word)
;; Go line begining
(global-set-key (kbd "M-a") 'beginning-of-visual-line)
;; Go to line end
(global-set-key (kbd "M-e") 'end-of-visual-line)
;; Go function begining
(global-set-key (kbd "C-a") 'beginning-of-defun)
;; Go function end
(global-set-key (kbd "C-e") 'end-of-defun)
;; Scroll up
(global-set-key (kbd "M-n") 'scroll-up-command) 
;; Scroll down
(global-set-key (kbd "M-b") 'scroll-down-command)
;; Begining of buffer
(global-set-key (kbd "M-,") 'beginning-of-buffer)
;; End of buffer
(global-set-key (kbd "M-.") 'end-of-buffer)
;; Backword list
(global-set-key (kbd "M-[") 'backward-list)
;; Forward list
(global-set-key (kbd "M-]") 'forward-list)

