;; File: ~/.emacs
;; Author: Gökmen Görgen, <gkmngrgn_gmail.com>

;; Define Paths
(setq load-path (cons "~/.elisp" load-path))
(progn (cd "~/.elisp") (normal-top-level-add-subdirs-to-load-path))

;; Environment
(setq default-directory "~/Repositories/")
(set-language-environment 'turkish)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Emacs Window Geometry
(add-to-list 'default-frame-alist '(height . 40))
(add-to-list 'default-frame-alist '(width . 130))

;; Disable emacs splash screen
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Indentation
(setq standart-indent 4)
(setq-default indent-tabs-mode nil)

;; Scrolling
(setq
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1)
(line-number-mode t)
(column-number-mode t)

;; Code Completions
(require 'anything)

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-ipython-complete
                                '(length initial-pattern)))

;; Theme and ScrollBar
(require 'color-theme)
(color-theme-initialize)

(if window-system
    (progn
      (color-theme-railscasts)
      (scroll-bar-mode -1)
      (tool-bar-mode -1))
  (color-theme-ld-dark))

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

;; Yaml Mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Python Customizations
(require 'python-mode)
(require 'python-pep8)

(autoload 'python-mode "python-mode" "Python Mode." t)
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'interpreter-mode-alist '("python" . python-mode))

;; Erlang Customizations
(setq exec-path (cons "/usr/local/bin" exec-path))
(require 'erlang-start)
(require 'erlang-flymake)

;; Scala Customizations
(require 'scala-mode)

(add-hook 'scala-mode-hook '(lambda () (yas/minor-mode-on)))
(autoload 'scala-mode "scala-mode" "Scala Mode." t)
(add-to-list 'auto-mode-alist '("\\.scala\\'" . scala-mode))
(add-to-list 'interpreter-mode-alist '("scala" . scala-mode))

;; Snippets
(require 'yasnippet-bundle)

(yas/initialize)
(yas/load-directory "~/.elisp/snippets/")

;; Apple Keyboard Fixes
(if (eq system-type 'darwin)
    (progn
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
      (global-set-key (kbd "M-ü") (lambda () (interactive) (insert "~")))))

;; General Keyboard Fixes
(global-set-key (kbd "C-x r C-SPC") 'rm-set-mark)
(global-set-key (kbd "C-x r C-x") 'rm-exchange-point-and-mark)
(global-set-key (kbd "C-x r C-w") 'rm-kill-region)
(global-set-key (kbd "C-x r M-w") 'rm-kill-ring-save)
(global-set-key (kbd "C-c g") 'goto-line)
(global-set-key (kbd "C-c t") 'sr-speedbar-toggle)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(autoload 'rm-set-mark "rect-mark" "Set mark for rectangle." t)
(autoload 'rm-exchange-point-and-mark "rect-mark" "Exchange point and mark for rectangle." t)
(autoload 'rm-kill-region "rect-mark" "Kill a rectangular region and save it in the kill ring." t)
(autoload 'rm-kill-ring-save "rect-mark" "Copy a rectangular region to the kill ring." t)

;; Speedbar Customization
(require 'sr-speedbar)
(setq speedbar-mode-hook '(lambda ()
                            (speedbar-toggle-images)
                            (speedbar-toggle-show-all-files)
                            ))