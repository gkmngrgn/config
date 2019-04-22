;;; init.el --- GKMNGRGN personal emacs configuration file.

;;; Commentary:
;;
;; Install Python environment dependencies with a pip command:
;;
;;   $ pip install -r requirements.txt
;;
;; Install `ag` for searching and refactoring
;;

;;; Code:

;; Encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

;; Packages
(require 'package)

(add-to-list 'package-archives
    '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(when (version< emacs-version "27.0")
    (package-initialize))
(when (not package-archive-contents)
    (package-refresh-contents))

(defvar my-packages
    '(company
         company-lsp
         dart-mode
         editorconfig
         ; emojify
         flx-ido
         flycheck
         go-mode
         helm
         helm-ag
         helm-ls-git
         js2-mode
         lsp-mode
         magit
         markdown-mode
         neotree
         scss-mode
         use-package
         web-mode)
    "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
    (when (not (package-installed-p p)) (package-install p)))

(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
    (message "All other buffers are killed.."))

;; Default settings
(setq-default
    indent-tabs-mode nil
    truncate-lines t)
(setq
    ido-enable-flex-matching t
    ido-use-faces nil
    inhibit-splash-screen t
    scroll-conservatively 10000
    scroll-step 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode t)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(electric-pair-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
; (ac-config-default)

;; GUI settings
(if (display-graphic-p)
    (progn
        (setq visible-bell 1)
        (scroll-bar-mode -1)
        (set-face-attribute 'default nil
            :font "Consolas"
            :height 115
            :weight 'normal
            :width 'normal)))

;; Keyboard shortcuts
(global-set-key [f8] 'neotree-toggle)
(global-set-key (kbd "C-c SPC") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c TAB") 'company-complete)
(global-set-key (kbd "C-c b") 'ibuffer)
(global-set-key (kbd "C-c f") 'helm-find-files)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c k") 'kill-other-buffers)
(global-set-key (kbd "C-c s") 'helm-ag-project-root)

;; Major mode customizations
(defalias 'yes-or-no-p 'y-or-n-p)

;; Place all backup files in one directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq default-directory (concat (getenv "HOME") "/Workspace"))

;; Sidebar
(require 'neotree)
(setq neo-theme 'arrow)
(setq neo-window-fixed-size 0)
(setq neo-window-width '30)

;; Common settings for all languages
(global-company-mode)
(global-flycheck-mode)
; (global-emojify-mode)
(global-auto-revert-mode)
; (global-hl-line-mode)

;; Web
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/some/react/path/.*\\.js[x]?\\'" . web-mode))
(setq web-mode-content-types-alist
    '(("json" . "/some/path/.*\\.api\\'")
	     ("xml"  . "/other/path/.*\\.api\\'")
	     ("jsx"  . "/some/react/path/.*\\.js[x]?\\'")))
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-engines-alist '(("django" . "\\.html?\\'")))
(setq web-mode-enable-auto-pairing nil)

;; Language Server Protocol
(use-package lsp-mode
    :commands lsp
    :init

    ;; I use flycheck instead.
    (setq lsp-prefer-flymake nil)

    (add-hook 'python-mode-hook #'lsp))

(use-package company-lsp :commands company-backends)

;; JavaScript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-pretty-multiline-decl-indentation-p 'non-nil)
(setq js2-consistent-level-indent-inner-bracket-p 'non-nil)
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

;; CSS
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq css-indent-offset 2)

;; Helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

;; Text file settings
(add-hook 'text-mode-hook 'visual-line-mode)

(use-package markdown-mode
    :defer t
    :mode (("\\.md?\\'" . markdown-mode))
    :config

    (set-face-attribute 'markdown-code-face nil
        :inherit nil
        :foreground "dim gray")
    (add-hook 'markdown-mode-hook 'visual-line-mode))

;; Editorconfig
(require 'editorconfig)
(editorconfig-mode 1)

;; Custom variables
(defvar custom-file-path "~/.emacs.d/custom.el")
(setq custom-file custom-file-path)
(when (file-exists-p custom-file-path)
    (load custom-file))

;;; Local Variables:
;; coding: utf-8
;; inten-tabs-mode: nil

;;; init.el ends here
