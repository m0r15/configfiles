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


;;; Load path for plugin
(if (system-is-windows)
    (add-to-list 'load-path "C:/.emacs.d/lisp/")
    (add-to-list 'load-path "~/.emacs.d/lisp/"))


;;; Package manager
;; Initial package and add Melpa repo
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(package-initialize) 

;; Package list
(defvar required-packages
  '(ergoemacs-mode
    smartparens
;    auto-complete
    zenburn-theme
;    color-theme-sanityinc-tomorrow
    Company
    Company-Racer
    Flycheck
    Flycheck-rust
    rust-mode
    racer
    emmet-mode))

;; Require Common Lisp extension (needed for loop)
(require 'cl)

(defun packages-installed-p ()
    "Packages availability checking."
    (interactive)
    (loop for package in required-packages 
          unless (package-installed-p package)
            do (return nil)
          finally (return t)))

;; Auto-install packages
(unless (packages-installed-p)
    (message "%s" "Emacs is now refreshing it's package database...")
    (package-refresh-contents)
    (message "%s" " done.")
    (dolist (package required-packages)
        (unless (package-installed-p package)
            (package-install package))))


;;; Settings pack
;; Dired
(require 'dired)
(setq dired-recursive-deletes 'top)

;; Imenu
(require 'imenu)
(setq imenu-auto-rescan      t)
(setq imenu-use-popup-menu nil)

;; Display the name of the current buffer in the title bar
(setq frame-title-format "GNU Emacs: %b")

;; Org-mode
(require 'org)
(add-to-list 'auto-mode-alist '("\\.org?" . org-mode)) ;; assoc with *.org

;; Inhibit startup/splash screen
(setq inhibit-splash-screen   t)
(setq ingibit-startup-message t)

;; Show-paren-mode
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; Electric-mode 
(electric-pair-mode   -1) ;; autoclose {},[],()
(electric-indent-mode -1)

;; Disable GUI tools and menu
(tooltip-mode                 -1)
(menu-bar-mode                -1)
(tool-bar-mode                -1)
(scroll-bar-mode              -1)
(blink-cursor-mode            -1)
(setq use-dialog-box         nil)
(setq redisplay-dont-pause     t) ;; more speed for drawing buff
;(setq-default cursor-type 'hbar)
(setq ring-bell-function 'ignore)

;; Fringe
(fringe-mode '(8 . 0)) ;; limit only left
(setq-default indicate-empty-lines t)
(setq-default indicate-buffer-boundaries 'left)

;; Set font
(when (member "Cousine" (font-family-list))
    (set-frame-font "Cousine-10:antialias=natural" nil t))

;; Disable backup/auto-save files
(setq make-backup-files        nil) ; Don't want any backup files
(setq auto-save-list-file-name nil) ; Don't want any .saves files
(setq auto-save-default        nil) ; Don't want any auto saving
(setq create-lockfiles         nil) ; Don't want any .# files

;; Coding-system
(set-language-environment 'UTF-8)
(if (system-is-linux)
    (progn
        (setq default-buffer-file-coding-system 'utf-8)
        (setq-default coding-system-for-read    'utf-8)
        (setq file-name-coding-system           'utf-8)
        (set-selection-coding-system            'utf-8)
        (set-keyboard-coding-system        'utf-8-unix)
        (set-terminal-coding-system             'utf-8)
        (prefer-coding-system                   'utf-8))
    (progn
        (prefer-coding-system                   'utf-8)
        (set-terminal-coding-system             'utf-8)
        (set-keyboard-coding-system        'utf-8-unix)
        (set-selection-coding-system            'utf-8)
        (setq file-name-coding-system           'utf-8)
        (setq-default coding-system-for-read    'utf-8)
        (setq default-buffer-file-coding-system 'utf-8)))

;; Line number activate
(require 'linum+)
(setq linum-format " %d ")
(global-linum-mode 1)

;; Display file size/date in mode-line
(setq display-time-24hr-format t)
(display-time-mode             t) ;; show clock in mode-line
(size-indication-mode          t) ;; size of file in %

;; Line wrapping
(setq word-wrap          t)
(global-visual-line-mode t)
(setq-default fill-column 80)

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

;; Syntax highlighting
(require 'font-lock)
(global-hl-line-mode               t)
(global-font-lock-mode             t)
(setq font-lock-maximum-decoration t)

;; Sr-speedbar http://www.emacswiki.org/emacs/SrSpeedbar
(require 'sr-speedbar)

;; Mark-down http://emacswiki.org/emacs/MarkdownMode
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md?" . markdown-mode))

;; Indentation
(setq-default indent-tabs-mode nil) ; off tab indent
(setq-default tab-width          4)
(setq-default c-basic-offset     4)
(setq-default standart-indent    4)
(setq-default lisp-body-indent   4)
(setq rust-indent-offset         4)
(setq lisp-indent-function 'common-lisp-indent-function)

;; Scrolling
(setq scroll-step               1)
(setq scroll-margin            10)
(setq scroll-conservatively 10000)

;; Short message
(defalias 'yes-or-no-p 'y-or-n-p)

;; Global clipboard
(setq x-select-enable-clipboard t)

;; Revert buffer
(global-auto-revert-mode t)

;; Filed end newlines
(setq require-final-newline t)
(setq next-line-add-newlines nil)

;; Highlight search results
(setq search-highlight        t)
(setq query-replace-highlight t)

;; Highlight expression in {},[],()
(setq show-paren-style 'expression)
(show-paren-mode 2)



;;; Plug-ins
(when (packages-installed-p)

    ;; Zenburn color theme
    (load-theme 'zenburn t)

    ;; Color-theme-sanityinc-tomorrow
;    (load-theme 'color-theme-sanityinc-tomorrow-bright)

    ;; Smartpaens
    (require 'smartparens-config)
    (smartparens-global-mode t)

    ;; Auto-complete
;    (require 'auto-complete)
;    (require 'auto-complete-config)
;    (ac-config-default)
;    (setq ac-auto-start t)
;    (setq ac-auto-show-menu t)
;    (global-auto-complete-mode t)
;    (add-to-list 'ac-modes 'lisp-mode)

    ;; ergoemacs
    (require 'ergoemacs-mode)
    (setq ergoemacs-theme "lvl2") ; Uses standard Ergoemacs keys
    (setq ergoemacs-keyboard-layout "us") ; QWERTY keyboard
    (ergoemacs-mode 1)

    ;; flycheck
    (add-hook 'after-init-hook #'global-flycheck-mode)

    ;; Company
    (global-company-mode)
    (setq company-idle-delay 0.2) ; the time after which company autocomplete
    (setq company-minimum-prefix-length 1) ; the number of char before company kicks in
    
    ;; Rust-mode
    (if (system-is-linux)
            (progn 
                (setq racer-rust-src-path "~/.local/share/rust-src/src/")
                (setq racer-cmd "~/bin/racer")))
    (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
    (add-hook 'rust-mode-hook
              '(lambda ()
                ;; Enable racer
                (racer-mode)
                ;; Hook in racer with eldoc to provide doc
                (racer-turn-on-eldoc)
                ;; Use flychecker-rust
                (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
                ;; Use company racer in rust mode
                (set (make-local-variable 'company-backends) '(company-racer))
                ;; Key bind to jump to method def
                (local-set-key (kbd "M-.") #'racer-find-definition)
                ;; Key bind to auto compl and indent
                (local-set-key (kbd "TAB") #'racer-complete-or-indent)))

    ;; Emmet-mode
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook 'emmet-mode))
    


;;; Key bindings:

;; Commands
;; bs key
(global-set-key (kbd "<f2>") 'bs-show)
;; Sr-speedbar
(global-set-key (kbd "<f12>") 'sr-speedbar-toggle)
;; Org-mode
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cl" 'org-store-link)
;; Newline and indent
(global-set-key (kbd "RET") 'newline-and-indent)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector (vector "#eaeaea" "#d54e53" "#b9ca4a" "#e7c547" "#7aa6da" "#c397d8" "#70c0b1" "#000000"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes (quote ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default)))
 '(fci-rule-color "#424242")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map (quote ((20 . "#d54e53") (40 . "#e78c45") (60 . "#e7c547") (80 . "#b9ca4a") (100 . "#70c0b1") (120 . "#7aa6da") (140 . "#c397d8") (160 . "#d54e53") (180 . "#e78c45") (200 . "#e7c547") (220 . "#b9ca4a") (240 . "#70c0b1") (260 . "#7aa6da") (280 . "#c397d8") (300 . "#d54e53") (320 . "#e78c45") (340 . "#e7c547") (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
