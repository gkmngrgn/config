;;; ~/.emacs.d/init.el --- GKMNGRGN personal emacs configuration file.

;;; Commentary:
;;
;; 1. Install OS dependencies: `apt install -y silversearcher-ag`
;; 2. Install Python dependencies: `pip install -r requirements.txt`
;; 3. Install Rust dependencies: `rustup component add rls rust-analysis rust-src`
;; 4. Install Golang dependencies: `go get -u golang.org/x/tools/cmd/gopls`
;;

;;; Code:

;; Initial Setup
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
;; (set-selection-coding-system 'utf-8)

(setq-default cursor-type 'box
              indent-tabs-mode nil
              truncate-lines t
              org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE")))

(setq initial-scratch-message ""
      inhibit-splash-screen t
      scroll-conservatively 10
      scroll-margin 7
      visible-bell 1)

(electric-pair-mode 1)
(electric-layout-mode 1)
(electric-indent-mode 1)

(menu-bar-mode -1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'visual-line-mode)

(which-function-mode t)

(global-hl-line-mode)
(global-auto-revert-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Place all backup files in one directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      default-directory (concat (getenv "HOME") "/Workspace"))

;; Custom variables
(defvar custom-file-path "~/.emacs.d/custom.el")
(setq custom-file custom-file-path)
(when (file-exists-p custom-file-path)
  (load custom-file))

;; Packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(global-set-key (kbd "C-c SPC") 'comment-or-uncomment-region)

;; GUI settings
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-face-attribute 'default nil
                          :font "Fira Code Retina"
                          :height 115
                          :weight 'normal
                          :width 'normal)
      (fringe-mode 12) ;; for HiDPI
      (set-face-attribute 'fringe nil :background nil)))

;; Package configurations
(use-package avy
  :ensure t
  :bind (("M-g g" . avy-goto-line)
         ("M-g f" . avy-goto-char)
         ("M-g h" . avy-goto-char-2))
  :config
  (avy-setup-default))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

(use-package ag
  :ensure t)

(use-package cyberpunk-theme
  :ensure t
  :config
  (set-face-attribute 'mode-line nil :box nil)
  (set-face-attribute 'mode-line-inactive nil :box nil)
  (load-theme 'cyberpunk t))

(use-package company
  :ensure t
  :bind ("C-c TAB" . company-complete)
  :diminish (company-mode . "comp")
  :config
  (global-company-mode))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :after company
  :init
  (setq company-lsp-async t
        company-lsp-enable-snippet t
        company-lsp-cache-candidates 'auto)
  :config
  (push 'company-lsp company-backends))

(use-package diff-hl
  :ensure t
  :bind (("M-p" . diff-hl-previous-hunk)
         ("M-n" . diff-hl-next-hunk))
  :config
  (global-diff-hl-mode)
  (if (not (display-graphic-p))
      (diff-hl-margin-mode)))

(use-package diminish
  :ensure t)

(use-package editorconfig
  :ensure t
  :diminish (editorconfig-mode . "ec")
  :config
  (setq editorconfig-exclude-modes
        '(common-lisp-mode
          emacs-lisp-mode
          lisp-mode
          web-mode))
  (editorconfig-mode 1))

(use-package flycheck
  :ensure t
  :diminish (flycheck-mode . "fc")
  :config
  (global-flycheck-mode))

(use-package focus
  :ensure t
  :bind (("C-c f" . focus-mode)))

(use-package linum-relative
  :ensure t
  :diminish
  :custom
  (linum-relative-backend 'display-line-numbers-mode))

(use-package linum-relative-mode
  :ensure linum-relative
  :hook prog-mode)

(use-package lsp-mode
  :ensure t
  :hook ((dart-mode   . lsp)
         (go-mode     . lsp)
         (python-mode . lsp)
         (rust-mode   . lsp))
  :commands lsp
  :init
  (setq-default lsp-prefer-flymake nil                ; flycheck is better
                lsp-enable-snippet nil                ; company is better
                lsp-pyls-plugins-pylint-enabled nil)) ; pycodestyle is better

(use-package lsp-treemacs
  :ensure t
  :commands lsp-treemacs-errors-list)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package magit
  :ensure t
  :bind ("C-c g" . magit-status))

(use-package projectile
  :ensure t
  :diminish (projectile-mode . "pr")
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package rainbow-delimiters-mode
  :ensure rainbow-delimiters
  :hook prog-mode)

(use-package telephone-line
  :ensure t
  :init
  (defface color-gray
    '((t (:foreground "black" :background "gray")))
    "telephone gray"
    :group 'telephone-line)

  (defface color-red
    '((t (:foreground "white" :background "red")))
    "telephone red"
    :group 'telephone-line)

  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (gray . (telephone-line-vc-segment
                   telephone-line-erc-modified-channels-segment
                   telephone-line-process-segment))
          (nil  . (telephone-line-minor-mode-segment
                   telephone-line-buffer-segment))))

  (setq telephone-line-rhs
        '((nil  . (telephone-line-misc-info-segment))
          (gray . (telephone-line-major-mode-segment))
          (red  . (telephone-line-airline-position-segment))))

  :config
  (add-to-list 'telephone-line-faces '(gray . (color-gray . mode-line-inactive)))
  (add-to-list 'telephone-line-faces '(red  . (color-red  . mode-line-inactive)))
  (telephone-line-mode t))

;; File modes
(use-package dart-mode
  :ensure t)

(use-package dockerfile-mode
  :ensure t)

(use-package elpy
  :ensure t
  :diminish (elpy-mode . "py")
  :init
  (elpy-enable)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(use-package go-mode
  :ensure t)

(use-package lisp-mode
  :diminish eldoc-mode)

(use-package markdown-mode
  :ensure t
  :mode (("\\.md?\\'" . markdown-mode))
  :config
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil
                      :foreground "dim gray")
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(use-package js2-mode
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config
  (add-hook 'js2-mode-hook
            '(lambda ()
               (setq js2-pretty-multiline-decl-indentation-p t
                     js2-consistent-level-indent-inner-bracket-p t
                     js2-basic-offset 2))))

(use-package powershell
  :ensure t)

(use-package rust-mode
  :ensure t)

(use-package scss-mode
  :ensure t
  :mode (("\\.scss$" . scss-mode)
         ("\\.sass$" . scss-mode)))

(use-package text-mode
  :diminish (visual-line-mode . "wrap")
  :init
  (add-to-list 'auto-mode-alist '("\\`/tmp/neomutt-" . mail-mode)))

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-engines-alist '(("django" . "\\.html$"))
        web-mode-enable-auto-pairing nil
        web-mode-block-padding 0))

(use-package yaml-mode
  :ensure t)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
