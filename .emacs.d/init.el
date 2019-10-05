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

(setq-default
 cursor-type 'box
 indent-tabs-mode nil
 truncate-lines t)

(setq
 initial-scratch-message ""
 inhibit-splash-screen t
 scroll-conservatively 10000
 scroll-step 1
 visible-bell 1)

(menu-bar-mode -1)

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'text-mode-hook 'visual-line-mode)

(which-function-mode t)

(global-hl-line-mode)
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
                         ("melpa-stable" . "https://melpa.org/packages/")))

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
    focus
    rainbow-delimiters

    ;; navigation plugins
    flx-ido
    helm
    helm-ag
    helm-flx
    helm-ls-git
    helm-lsp

    ;; language server protocol
    company
    company-lsp
    company-flx
    flycheck
    lsp-mode
    lsp-ui
    lsp-treemacs)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p)) (package-install p)))

(global-set-key (kbd "C-c SPC") 'comment-or-uncomment-region)

(use-package cyberpunk-theme
  :config
  (load-theme 'cyberpunk t)
  (set-face-attribute 'mode-line nil
                      :box nil)
  (set-face-attribute 'mode-line-inactive nil
                      :box nil))

;; GUI settings
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (tool-bar-mode -1)
      (set-face-attribute 'default nil
                          :font "Fira Code"
                          :height 110
                          :weight 'normal
                          :width 'normal)
      (set-face-attribute 'fringe nil :background nil)))

;; Package Configurations
(setq-default
 org-todo-keywords '((sequence "TODO" "INPROGRESS" "|" "DONE")))

(electric-pair-mode 1)
(electric-layout-mode 1)
(electric-indent-mode 1)
;; (electric-quote-mode 1)

(use-package flx-ido
  :config
  (ido-mode 1)
  ;; (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil))

(use-package rainbow-delimiters-mode
  :defer t
  :hook prog-mode)

(use-package ace-window
  :defer t
  :bind ("M-o" . ace-window))

(use-package avy
  :defer t
  :bind (("M-g g" . avy-goto-line)
         ("M-g f" . avy-goto-char)
         ("M-g h" . avy-goto-char-2))
  :config
  (avy-setup-default))

(use-package magit
  :defer t
  :bind ("C-c g" . magit-status))

(use-package git-gutter
  :defer t
  :bind (("M-p" . git-gutter:previous-hunk)
         ("M-n" . git-gutter:next-hunk))
  :diminish
  :config
  (global-git-gutter-mode))

(use-package focus
  :defer t
  :bind (("C-c f" . focus-mode)))

(use-package helm-mode
  :bind (("M-s o"   . helm-occur)
         ("M-x"     . helm-M-x)
         ("C-c s"   . helm-ag-project-root)
         ("C-c t"   . helm-imenu)
         ("C-x C-b" . helm-buffers-list)
         ("C-x C-d" . helm-browse-project))
  :config
  (helm-flx-mode +1))

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

  (setq telephone-line-faces
        '((gray   . (color-gray . mode-line-inactive))
          (red    . (color-red . mode-line-inactive))
          (evil   . telephone-line-evil-face)
          (accent . (telephone-line-accent-active . telephone-line-accent-inactive))
          (nil    . (mode-line . mode-line-inactive))))

  (setq telephone-line-lhs
        '((red  . (telephone-line-vc-segment))
          (gray . (telephone-line-buffer-segment))
          (nil  . (telephone-line-minor-mode-segment
                   telephone-line-erc-modified-channels-segment))))

  (setq telephone-line-rhs
        '((nil  . (telephone-line-misc-info-segment))
          (gray . (telephone-line-major-mode-segment))
          (red  . (telephone-line-airline-position-segment))))

  :config
  (telephone-line-mode t))

(use-package lsp-mode
  :hook ((dart-mode   . lsp)
         (go-mode     . lsp)
         (python-mode . lsp)
         (rust-mode   . lsp))
  :commands lsp
  :init
  (setq-default
   lsp-prefer-flymake nil                ; flycheck is better
   lsp-enable-snippet nil                ; company is better
   lsp-pyls-plugins-pylint-enabled nil)) ; pycodestyle is better

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package company
  :bind ("C-c TAB" . company-complete)
  :diminish (company-mode . "comp")
  :config
  (global-company-mode)
  (company-flx-mode +1))

(use-package company-lsp
  :commands company-lsp)

(use-package helm-lsp
  :commands helm-lsp-workspace-symbol)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list)

(use-package flycheck
  :diminish (flycheck-mode . "fc")
  :config
  (global-flycheck-mode))

;; programming languages
(use-package lisp-mode
  :diminish eldoc-mode)

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

;; web modes
(use-package web-mode
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-engines-alist '(("django" . "\\.html$"))
        web-mode-enable-auto-pairing nil
        web-mode-block-padding 0))

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

;; text modes
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

;;; init.el ends here

;; Local Variables:
;; coding: utf-8
