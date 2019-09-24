;;; init.el --- GKMNGRGN personal emacs configuration file.

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

(setq-default
 cursor-type 'bar
 indent-tabs-mode nil
 truncate-lines t)

(setq
 initial-scratch-message ""
 inhibit-splash-screen t
 scroll-conservatively 10000
 scroll-step 1
 visible-bell 1)

(menu-bar-mode -1)
(global-hl-line-mode)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'visual-line-mode)

(global-set-key (kbd "C-c SPC") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c b") 'ibuffer)

(global-auto-revert-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

;; Place all backup files in one directory
(setq
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 default-directory (concat (getenv "HOME") "/Workspace"))

;; Custom variables
(defvar custom-file-path "~/.emacs.d/custom.el")
(setq custom-file custom-file-path)
(when (file-exists-p custom-file-path)
  (load custom-file))

;; Packages
(require 'package)

(package-initialize)

(setq package-archives '(("gnu"          . "https://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(;; file modes
    dockerfile-mode
    dart-mode
    go-mode
    js2-mode
    markdown-mode
    powershell
    rust-mode
    scss-mode
    web-mode
    yaml-mode

    ;; common
    avy
    ace-window
    cyberpunk-theme
    diminish
    editorconfig
    git-gutter
    magit
    use-package
    telephone-line

    ;; navigation plugins
    flx-ido
    helm
    helm-ag
    helm-ls-git

    ;; language server protocol
    company
    company-lsp
    flycheck
    lsp-mode
    lsp-ui)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p)) (package-install p)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (message "All other buffers are killed.."))

(global-set-key (kbd "C-c k") 'kill-other-buffers)

(load-theme 'cyberpunk t)

;; GUI settings
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-face-attribute 'default nil
                          :font "Fira Code"
                          :height 100
                          :weight 'normal
                          :width 'normal)))

;; Package Configurations
(setq-default
 org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE")))

(electric-pair-mode 1)
(electric-layout-mode 1)
(electric-indent-mode 1)
(electric-quote-mode 1)

(use-package flx-ido
  :config
  (setq
   ido-enable-flex-matching t
   ido-use-faces nil)
  (ido-mode 1) ; TODO: do we need ido-mode?
  (ido-everywhere 1)
  (flx-ido-mode 1))

(use-package ace-window
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package avy
  :config
  (avy-setup-default)
  (global-set-key (kbd "M-g g") 'avy-goto-line)
  (global-set-key (kbd "C-;") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2))

(use-package git-gutter
  :diminish
  :config
  (global-git-gutter-mode)
  (global-set-key (kbd "M-p") 'git-gutter:previous-hunk)
  (global-set-key (kbd "M-n") 'git-gutter:next-hunk))

(use-package telephone-line
  :init

  (defface color-gray
    '((t (:foreground "black" :background "gray")))
    "telephone gray"
    :group 'telephone-line)

  (defface color-red
    '((t (:foreground "white" :background "red")))
    "telephone red"
    :group 'telephone-line)

  (telephone-line-defsegment arrow "GKMN")

  (setq telephone-line-primary-right-separator 'telephone-line-abs-left
        telephone-line-secondary-right-separator 'telephone-line-abs-left)

  (setq telephone-line-faces
        '((gray   . (color-gray . mode-line-inactive))
          (red    . (color-red . mode-line-inactive))
          (evil   . telephone-line-evil-face)
          (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
          (nil    . (mode-line . mode-line-inactive))))

  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (red  . (telephone-line-vc-segment))
          (gray . (telephone-line-buffer-segment))
          (nil  . (telephone-line-minor-mode-segment
                   telephone-line-erc-modified-channels-segment))))

  (setq telephone-line-rhs
        '((nil  . (telephone-line-misc-info-segment))
          (gray . (telephone-line-major-mode-segment))
          (red  . (telephone-line-airline-position-segment))
          (evil . (arrow))))

  :config
  (telephone-line-mode t))

(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-engines-alist '(("django" . "\\.html$"))
        web-mode-enable-auto-pairing nil))

(use-package lsp-mode
  :commands lsp
  :init

  (setq-default
   lsp-prefer-flymake nil               ; flycheck is better
   lsp-enable-snippet nil               ; company is better
   lsp-pyls-plugins-pylint-enabled nil) ; pycodestyle is better

  (add-hook 'dart-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'rust-mode-hook #'lsp)

  :config
  (use-package lsp-ui
    :commands lsp-ui-mode)
  (use-package company-lsp
    :commands company-lsp))

(use-package company
  :bind ("C-." . company-complete)
  :diminish
  :config
  (global-company-mode))

(use-package flycheck
  :diminish (flycheck-mode . "fc")
  :config
  (global-flycheck-mode))

(use-package js2-mode
  :mode ("\\.js$" . js2-mode)
  :config
  (add-hook 'js2-mode-hook
            '(lambda ()
               (setq
                js2-pretty-multiline-decl-indentation-p t
                js2-consistent-level-indent-inner-bracket-p t
                js2-basic-offset 2))))

(use-package scss-mode
  :mode (("\\.scss$" . scss-mode)
         ("\\.sass$" . scss-mode)))

(use-package helm-mode
  :config
  (global-set-key (kbd "M-s o") 'helm-occur)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c f") 'helm-find-files)
  (global-set-key (kbd "C-c s") 'helm-ag-project-root)
  (global-set-key (kbd "C-c t") 'helm-imenu)
  (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
  (global-set-key (kbd "C-x C-d") 'helm-browse-project))

(use-package lisp-mode
  :diminish eldoc-mode)

(use-package text-mode
  :diminish (visual-line-mode . "wrap"))

(use-package markdown-mode
  :defer t
  :mode (("\\.md?\\'" . markdown-mode))
  :config
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil
                      :foreground "dim gray")
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(use-package editorconfig
  :ensure t
  :diminish (editorconfig-mode . "ed")
  :config
  (setq editorconfig-exclude-modes
        '(lisp-mode emacs-lisp-mode common-lisp-mode))
  (editorconfig-mode 1))

(use-package magit
  :bind ("C-c g" . magit-status))

;;; Local Variables:
;; coding: utf-8
;; inten-tabs-mode: nil

;;; init.el ends here
