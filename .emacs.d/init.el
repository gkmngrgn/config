;; Gökmen Görgen: Hi, this is my Emacs configuration file.


;; encoding
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)

;; packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(when (version< emacs-version "27.0") (package-initialize))
(when (not package-archive-contents) (package-refresh-contents))
(defvar my-packages
  '(auto-complete
    editorconfig
    flx-ido
    helm
    helm-ag
    helm-ls-git
    js2-mode
    magit
    markdown-mode
    markdown-mode+
    scss-mode
    spacemacs-theme
    web-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p)) (package-install p)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list)))
  (message "All other buffers are killed.."))

;; default settings
(setq-default truncate-lines t)
(setq inhibit-splash-screen t)
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (scroll-bar-mode -1)
      (setq visible-bell 1)
      (load-theme 'spacemacs-dark t)))
(tool-bar-mode -1)
(column-number-mode t)

(set-face-attribute 'default nil :font "Consolas" :height 105 :weight 'normal :width 'normal)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)
(setq-default indent-tabs-mode nil)
(electric-pair-mode 1)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(ac-config-default)

;; keyboard shortcuts
(global-set-key (kbd "C-c k") 'kill-other-buffers)
(global-set-key (kbd "C-c SPC") 'comment-or-uncomment-region)
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; major mode customizations
(defalias 'yes-or-no-p 'y-or-n-p)

;; place all backup files in one directory
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq default-directory (concat (getenv "HOME") "/Workspace"))

;; web
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

;; javascript
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(setq js2-pretty-multiline-decl-indentation-p 'non-nil)
(setq js2-consistent-level-indent-inner-bracket-p 'non-nil)
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))

;; css
(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
(setq css-indent-offset 2)

;; helm
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-d") 'helm-browse-project)

;; text file settings
(add-hook 'markdown-mode-hook 'visual-line-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

;; editorconfig
(require 'editorconfig)
(editorconfig-mode 1)
