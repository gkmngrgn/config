;; File: ~/.emacs
;; Author: Gökmen Görgen, <gkmngrgn_gmail.com>

;; Define Paths
(setq load-path (cons "~/.elisp" load-path))
(progn (cd "~/.elisp") (normal-top-level-add-subdirs-to-load-path))

;; Font Size
(set-face-attribute 'default (selected-frame) :height 105)

;; Environment
(setq default-directory "~/Repositories/")
(set-language-environment 'turkish)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Emacs Window Geometry
(add-to-list 'default-frame-alist '(height . 50))
(add-to-list 'default-frame-alist '(width . 140))

;; Disable emacs splash screen
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Indentation
(setq standart-indent 4)
(setq-default indent-tabs-mode nil)

;; Autopair
(require 'autopair)
(autopair-global-mode)

;; Scrolling
(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)
(line-number-mode t)
(column-number-mode t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)
(setq scroll-step 1)

;; Code Completions
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.elisp/auto-complete/ac-dict")
(ac-config-default)

;; Theme and ScrollBar
(require 'color-theme)
(require 'color-theme-solarized)
(color-theme-solarized-dark)

(if window-system
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)))

;; Major Mode Customization
(setq-default fill-column 80)
(setq auto-fill-mode 1)
(setq default-major-mode 'text-mode)
(setq initial-scratch-message nil)

;; Hidden Widgets
(menu-bar-mode nil)
(line-number-mode t)

;; Autosave & Backup
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;; Remove White Spaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Environments
(setenv "PATH"
        (concat (getenv "HOME") "/local/node/bin:"
                (getenv "HOME") "/local/node/lib/node_modules/.bin:"
                (getenv "PATH")))


;; Google Translate
(require 'gtranslate)
(autoload 'gtranslate-translate "gtranslate" nil t)
(autoload 'gtranslate-translate-auto "gtranslate" nil t)

(defun translate-en-tr ()
  (interactive)
  (gtranslate-translate (gtranslate-region-or-input) "en"  "tr"))

(defun translate-tr-en ()
  (interactive)
  (gtranslate-translate (gtranslate-region-or-input) "tr" "en"))

(global-set-key "\M-1" 'translate-en-tr)
(global-set-key "\M-2" 'translate-tr-en)

;; Snippets
(require 'yasnippet-bundle)

(yas/initialize)
(yas/load-directory "~/.elisp/snippets/")

;; Yaml Mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Css / Less Mode
(require 'less-mode)
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . less-mode))

;; Django Mode
(require 'django-html-mode)
(require 'django-mode)
(yas/load-directory "~/.elisp/django-mode/snippets")

;; Html Mode
(add-to-list 'auto-mode-alist '("\\.shtml$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.htm$" . html-mode))
(add-hook 'html-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)))
(add-hook 'sgml-mode-hook
          (lambda ()
            (set (make-local-variable 'sgml-basic-offset) 4)
            (sgml-guess-indent)))

;; PHP Mode
(require 'php-mode)

;; Javascript and Coffeescript Mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-use-ast-for-indentation-p 'non-nil)

(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
(defun coffee-custom () "coffee-mode-hook"
  (set (make-local-variable 'tab-width) 4))

(add-hook 'coffee-mode-hook
          '(lambda() (coffee-custom)))

;; Python Customizations
(require 'python-mode)
(setq auto-mode-alist (cons '("\\.py$" . python-mode) auto-mode-alist))
(setq interpreter-mode-alist (cons '("python" . python-mode) interpreter-mode-alist))
(autoload 'python-mode "python-mode" "Python editing mode." t)

;; Erlang Customizations
(setq exec-path (cons "/usr/local/bin" exec-path))
(require 'erlang-start)
(require 'erlang-flymake)

;; Git Extension
(require 'magit)

;; Mac Customizations
(if (eq system-type 'darwin)
    (progn
      ;; Apple Keyboard Fixes
      (setq mac-option-key-is-meta nil)
      (setq mac-command-key-is-meta t)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier nil)

      (global-set-key (kbd "M-3") (lambda () (interactive) (insert "#")))
      (global-set-key (kbd "M-4") (lambda () (interactive) (insert "$")))
      (global-set-key (kbd "M-7") (lambda () (interactive) (insert "{")))
      (global-set-key (kbd "M-8") (lambda () (interactive) (insert "[")))
      (global-set-key (kbd "M-9") (lambda () (interactive) (insert "]")))
      (global-set-key (kbd "M-0") (lambda () (interactive) (insert "}")))
      (global-set-key (kbd "M--") (lambda () (interactive) (insert "|")))
      (global-set-key (kbd "M-q") (lambda () (interactive) (insert "@")))
      (global-set-key (kbd "M-*") (lambda () (interactive) (insert "\\")))
      (global-set-key (kbd "M-ü") (lambda () (interactive) (insert "~")))

      ;; Fix paths
      (setenv "PATH" (concat "/usr/bin:/usr/local/bin:/bin:/usr/sbin:/sbin:" (getenv "PATH")))
      )
  )

;; General Keyboard Fixes
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)

(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c w") 'delete-trailing-whitespace)

;; Magit Shortcuts
(global-set-key (kbd "C-c C-s") 'magit-status)

(autoload 'rm-set-mark "rect-mark" "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark" "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark" "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark" "Copy a rectangular region to the kill ring." t)
