;;; ~/.emacs.d/init.el --- GKMNGRGN personal emacs configuration file.

;; Copyright (c) 2010-2020 Gökmen Görgen
;;
;; Author: Gökmen Görgen <gkmngrgn@gmail.com>
;; URL: https://github.com/gkmngrgn/config

;;; Commentary:

;; OS:
;; - silversearcher-ag: a code searching tool.
;; - fonts-ibm-plex: my default font.
;;
;; Python:
;; I recommend you to use pyenv for installing packages without root access.
;;
;; $ pyenv install 3.8.1
;; $ pyenv global 3.8.1
;; $ pip install -U pip
;; $ pip install python-language-server[all]
;;
;; Rust:
;; Install rust with "rustup".  Then type this command:
;; $ rustup component add rls rust-analysis rust-src
;;
;; Dart:
;; Install Dart SDK, it has a builtin analysis tool.  Then customize SDK path in your editor.
;;
;; Golang:
;; $ go get -u golang.org/x/tools/cmd/gopls

;;; Code:

;; Initial Setup
(setq-default cursor-type 'box
              fill-column 80
              indent-tabs-mode nil
              truncate-lines t)

(setq initial-scratch-message ""
      inhibit-splash-screen t
      visible-bell t
      require-final-newline t
      line-number-mode t
      column-number-mode t)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook   'visual-line-mode)
(add-hook 'prog-mode-hook   'display-line-numbers-mode)

(menu-bar-mode -1)
(global-hl-line-mode)
(global-auto-revert-mode)
(prefer-coding-system       'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment   'utf-8)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Place all backup files in one directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      default-directory "~/")

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
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; GUI settings
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-frame-font "IBM Plex Mono Italic")
      (set-face-attribute 'default nil :height 120)))

;; Package configurations
(use-package avy
  :bind (("M-g g" . avy-goto-char-2)
         ("M-g f" . avy-goto-char))
  :config
  (avy-setup-default)
  :ensure t)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :ensure t)

(use-package ag
  :ensure t)

(use-package company
  :config
  (setq company-idle-delay nil
        company-show-numbers nil
        company-tooltip-limit 5
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above nil)
  :diminish (company-mode . "cmp")
  :ensure t
  :init
  (global-company-mode 1))

(use-package company-lsp
  :after company
  :bind ("C-<tab>" . company-complete-common)
  :commands (company-lsp)
  :config
  (push 'company-lsp company-backends)
  :ensure t
  :init
  (setq company-lsp-async t
        company-lsp-enable-snippet t
        company-lsp-cache-candidates 'auto))

(use-package company-prescient
  :after company
  :config
  (company-prescient-mode t)
  :defer t
  :ensure t)

(use-package company-solidity
  :after solidity-mode
  :defer t
  :ensure t)

(use-package counsel
  :bind (("M-x"     . counsel-M-x)
         ("C-r"     . counsel-git-grep)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-d" . counsel-git))
  :ensure t)

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (if (not (display-graphic-p))
      (diff-hl-margin-mode))
  :ensure t)

(use-package diminish
  :ensure t)

(use-package editorconfig
  :ensure t
  :diminish (editorconfig-mode . "edc")
  :config
  (setq editorconfig-exclude-modes
        '(common-lisp-mode
          emacs-lisp-mode
          lisp-mode
          web-mode))
  (editorconfig-mode 1))

(use-package exec-path-from-shell
  :ensure t
  :commands (exec-path-from-shell-initialize)
  :init
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package expand-region
  :bind (("C-M-w" . er/expand-region))
  :defer t
  :ensure t)

(use-package flycheck
  :diminish (flycheck-mode . "fly")
  :ensure t
  :init
  (global-flycheck-mode))

(use-package flycheck-rust
  :ensure t
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package focus
  :defer t
  :ensure t)

(use-package indent-guide
  :defer t
  :ensure t
  :init
  (setq indent-guide-delay 0.2)
  (indent-guide-global-mode))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  :diminish
  :ensure t)

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode t)
  :defer t
  :ensure t)

(use-package lsp-ivy
  :after counsel
  :commands (lsp-ivy-workspace-symbol)
  :defer t
  :ensure t)

(use-package lsp-mode
  :commands (lsp)
  :diminish (lsp-mode . "lsp")
  :ensure t
  :hook ((dart-mode   . lsp-deferred)
         (go-mode     . lsp-deferred)
         (python-mode . lsp-deferred)
         (rust-mode   . lsp-deferred))
  :init
  (setq-default lsp-prefer-flymake nil   ; flycheck is better
                lsp-enable-snippet nil)) ; company is better

(use-package lsp-treemacs
  :commands (lsp-treemacs-errors-list)
  :defer t
  :ensure t)

(use-package lsp-ui
  :defer t
  :ensure t
  :commands (lsp-ui-mode)
  :init
  (setq lsp-ui-doc-enable nil))

(use-package magit
  :bind (("C-x g" . magit-status))
  :defer t
  :ensure t)

(use-package modus-operandi-theme
  :ensure t)

(use-package modus-vivendi-theme
  :ensure t)

(use-package org
  :init
  (setq org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE")))
  (setq org-log-done t))

(use-package org-bullets
  :defer t
  :ensure t
  :hook (org-mode . org-bullets-mode))

(use-package prescient
  :commands (prescient-persist-mode)
  :config
  (prescient-persist-mode t)
  :defer t
  :ensure t)

(use-package projectile
  :defer t
  :ensure t
  :diminish (projectile-mode . "prj")
  :config
  (projectile-mode +1)
  :init
  (setq projectile-completion-system 'ivy))

(use-package rainbow-delimiters-mode
  :ensure rainbow-delimiters
  :hook prog-mode)

(use-package smartparens
  :diminish
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t)
  (show-smartparens-global-mode t)
  :ensure t)

(use-package slime-company
  :ensure t)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (slime-setup '(slime-fancy slime-company))
  :diminish (slime-mode . "slm")
  :ensure t)

(use-package solidity-mode
  :defer t
  :ensure t)

(use-package solidity-flycheck
  :after solidity-mode
  :defer t
  :ensure t)

(use-package swiper
  :bind (("C-s" . swiper))
  :ensure t)

(use-package undo-fu
  :defer t
  :ensure t)

(use-package yasnippet
  :config
  (yas-global-mode 1)
  :diminish (yasnippet-mode . "yas")
  :ensure t)

(use-package yasnippet-snippets
  :after yasnippet
  :ensure t)

;; File modes
(use-package dart-mode
  :defer t
  :ensure t)

(use-package dockerfile-mode
  :defer t
  :ensure t)

(use-package go-mode
  :defer t
  :ensure t)

(use-package lisp-mode
  :defer t
  :diminish eldoc-mode)

(use-package markdown-mode
  :defer t
  :ensure t
  :mode (("\\.md$" . markdown-mode))
  :config
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil
                      :foreground "dim gray")
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(use-package js2-mode
  :defer t
  :ensure t
  :mode ("\\.js$" . js2-mode)
  :config
  (add-hook 'js2-mode-hook
            '(lambda ()
               (setq js2-pretty-multiline-decl-indentation-p t
                     js2-consistent-level-indent-inner-bracket-p t
                     js2-basic-offset 2))))

(use-package powershell
  :defer t
  :ensure t)

(use-package rust-mode
  :defer t
  :ensure t
  :config
  (use-package cargo
    :diminish (cargo-minor-mode . "crg")
    :ensure t
    :hook (rust-mode . cargo-minor-mode)))

(use-package scss-mode
  :defer t
  :ensure t
  :mode (("\\.scss$" . scss-mode)
         ("\\.sass$" . scss-mode)))

(use-package text-mode
  :diminish (visual-line-mode . "wrp")
  :init
  (add-to-list 'auto-mode-alist '("\\`/tmp/neomutt-" . mail-mode)))

(use-package web-mode
  :defer t
  :ensure t
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-engines-alist '(("django" . "\\.html$"))
        web-mode-enable-auto-pairing nil
        web-mode-block-padding 0
        web-mode-enable-auto-indentation nil))

(use-package yaml-mode
  :defer t
  :ensure t)

;; Hydra settings
(use-package hydra
  :bind (("C-c e" . hydra-errors/body)
         ("C-c f" . hydra-focus/body)
         ("C-c l" . hydra-lsp/body)
         ("C-c p" . hydra-project/body)
         ("C-c s" . hydra-yasnippet/body))
  :config
  (with-no-warnings ;; to ignore the warning message "the following functions might not be defined..."
    (defhydra hydra-errors (:pre (flycheck-list-errors)
                                 :post (quit-windows-on "*Flycheck errors*")
                                 :hint nil)
      "Errors"
      ("f"   flycheck-error-list-set-filter      "Filter")
      ("j"   flycheck-next-error                 "Next")
      ("k"   flycheck-previous-error             "Previous")
      ("RET" nil                                 "Close" :color blue))

    (defhydra hydra-focus (:columns 4)
      "Focus"
      ("g"   text-scale-increase                 "Zoom in")
      ("l"   text-scale-decrease                 "Zoom out")
      ("f"   focus-mode                          "Focus")
      ("r"   focus-read-only-mode                "Review")

      ("["   diff-hl-previous-hunk               "Previous diff")
      ("="   diff-hl-diff-goto-hunk              "Show diff")
      ("]"   diff-hl-next-hunk                   "Next diff")
      ("-"   diff-hl-revert-hunk                 "Revert changes")

      ("u"   undo-fu-only-undo                   "Undo")
      ("U"   undo-fu-only-redo                   "Redo")
      ("RET" nil                                 "Close" :color blue))

    (defhydra hydra-project (:columns 4)
      "Projectile"
      ("f"   projectile-find-file                "Find file")
      ("r"   projectile-recentf                  "Recent files")
      ("z"   projectile-cache-current-file       "Cache current file")
      ("x"   projectile-remove-known-project     "Remove known project")

      ("d"   projectile-find-dir                 "Find directory")
      ("b"   projectile-switch-to-buffer         "Switch to buffer")
      ("c"   projectile-invalidate-cache         "Clear cache")
      ("X"   projectile-cleanup-known-projects   "Cleanup known projects")

      ("o"   projectile-multi-occur              "Multi occur")
      ("s"   projectile-switch-project           "Switch project")
      ("k"   projectile-kill-buffers             "Kill buffers")
      ("RET" nil                                 "Close" :color blue))

    (defhydra hydra-yasnippet (:columns 4)
      "Yasnippet"
      ("d" yas-load-directory                    "Load directory")
      ("i" yas-insert-snippet                    "Insert snippet")
      ("f" yas-visit-snippet-file                "Visit snippet file")
      ("n" yas-new-snippet                       "New snippet")

      ("g" yas/global-mode                       "Global mode")
      ("m" yas/minor-mode                        "Minor mode")
      ("e" yas-activate-extra-mode               "Extra mode")
      ("a" yas-reload-all                        "Reload all")

      ("t" yas-tryout-snippet                    "Tryout snippet")
      ("l" yas-describe-tables                   "Describe tables")
      ("RET" nil                                 "Close" :color blue))

    (defhydra hydra-lsp (:columns 4)
      "LSP"
      ("d"   lsp-find-declaration                "Find declaration")
      ("D"   lsp-ui-peek-find-definitions        "Find definitions")
      ("R"   lsp-ui-peek-find-references         "Find references")
      ("i"   lsp-ui-peek-find-implementation     "Find implementation")

      ("t"   lsp-find-type-definition            "Find type definition")
      ("r"   lsp-rename                          "Rename")
      ("f"   lsp-format-buffer                   "Format buffer")
      ("m"   lsp-ui-imenu                        "Menu")

      ("a"   lsp-ui-sideline-apply-code-actions  "Select action")
      ("M-s" lsp-describe-session                "Describe session")
      ("M-r" lsp-workspace-restart               "Restart workspace")
      ("RET" nil                                 "Close" :color blue)))
  :ensure t)

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
