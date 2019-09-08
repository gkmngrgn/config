;;; init.el --- GKMNGRGN personal emacs configuration file.

;;; Commentary:
;;
;; 1. Install OS dependencies: `apt install -y emacs-nox silversearcher-ag`
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
 indent-tabs-mode nil
 truncate-lines t)

(setq
 inhibit-splash-screen t
 scroll-conservatively 10000
 scroll-step 1
 visible-bell 1)

(menu-bar-mode -1)
(line-number-mode t)
(column-number-mode t)
(electric-pair-mode 1)
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

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")))

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    ;; file modes
    dockerfile-mode
    dart-mode
    go-mode
    js2-mode
    markdown-mode
    rust-mode
    scss-mode
    web-mode
    yaml-mode

    ;; common
    editorconfig
    magit
    use-package
    cyberpunk-theme

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

;; GUI settings
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (global-hl-line-mode)
      (load-theme 'cyberpunk t)
      (if (eq system-type 'windows-nt)
          (set-face-attribute 'default nil
                              :font "Consolas"
                              :height 115
                              :weight 'normal
                              :width 'normal))))

;; Package Configurations
(use-package flx-ido
  :config
  (setq
   ido-enable-flex-matching t
   ido-use-faces nil)
  (ido-mode 1) ; TODO: do we need ido-mode?
  (ido-everywhere 1)
  (flx-ido-mode 1))

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

  (setq
   lsp-prefer-flymake nil  ; I use flycheck instead.
   lsp-enable-snippet nil) ; I use company instead.

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
  :bind ("C-c TAB" . company-complete)
  :config
  (global-company-mode))

(use-package flycheck
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

(use-package css-mode
  :config
  (setq css-indent-offset 2))

(use-package scss-mode
  :mode (("\\.scss$" . scss-mode)
         ("\\.sass$" . scss-mode)))

(use-package helm-mode
  :config
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c f") 'helm-find-files)
  (global-set-key (kbd "C-c s") 'helm-ag-project-root)
  (global-set-key (kbd "C-x C-d") 'helm-browse-project))

(use-package markdown-mode
  :defer t
  :mode (("\\.md?\\'" . markdown-mode))
  :config
  (set-face-attribute 'markdown-code-face nil
                      :inherit nil
                      :foreground "dim gray")
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package magit
  :bind ("C-c g" . magit-status))

;;; Local Variables:
;; coding: utf-8
;; inten-tabs-mode: nil

;;; init.el ends here
