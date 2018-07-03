;;------------------------------------------------------------------------
;; Get info about environment
;;------------------------------------------------------------------------

(defconst is-mac (memq window-system '(mac ns)))
(defconst backup-directory (if is-mac "~/.Trash" "~/.saves"))

;;------------------------------------------------------------------------
;; Remove unnecessary things
;;------------------------------------------------------------------------

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(when (fboundp 'blink-cursor-mode)
  (blink-cursor-mode -1))

(if is-mac
    (setq mac-option-modifier 'meta
          mac-command-modifier 'super)
  (when (fboundp 'menu-bar-mode)
    (menu-bar-mode -1)))

;;------------------------------------------------------------------------
;; Speed up emacs loading
;; Thanks jwiegly!
;;------------------------------------------------------------------------

(defvar file-name-handler-alist-old file-name-handler-alist)

(setq package-enable-at-startup nil
      file-name-handler-alist nil
      message-log-max 16384
      gc-cons-threshold 402653184
      gc-cons-percentage 0.6
      auto-window-vscroll nil)

(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist file-name-handler-alist-old
                   gc-cons-threshold 800000
                   gc-cons-percentage 0.1)
             (garbage-collect)) t)

;;------------------------------------------------------------------------
;; Bootstrap use-package
;;------------------------------------------------------------------------

(eval-when-compile
  (require 'package)
  (add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package))
  (require 'use-package))
(use-package diminish
  :ensure t)
(use-package bind-key
  :ensure t)

;;------------------------------------------------------------------------
;; Settings
;;------------------------------------------------------------------------

(setq backup-directory-alist `(("." . ,backup-directory))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5
      ;; Tabs
      indent-tabs-mode nil)

(fset `yes-or-no-p `y-or-n-p)

;;------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------

(use-package aggressive-indent
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode) . aggressive-indent-mode)
  :diminish aggressive-indent-mode)

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char)
	 ("M-g g" . avy-goto-line)
	 ("C-'" . avy-goto-word-0)))

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t
  :mode (("\\.edn$" . clojure-mode)))

(use-package company               
  :ensure t
  :defer t
  :init (global-company-mode)
  :config
  (setq company-idle-delay 0.15
	company-tooltip-limit 20
	company-echo-delay 0
	company-begin-commands '(self-insert-command)
	company-show-numbers t)
  :diminish company-mode)

(use-package elpy
  :ensure t
  :config
  (elpy-enable))

(use-package erc
  :ensure t
  :commands (erc erc-tls))

(use-package exec-path-from-shell
  :if is-mac
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package helm
  :ensure t
  :defer t
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-i"   . helm-execute-persistent-action)
         ("C-z"   . helm-select-action))
  :config
  (helm-autoresize-mode t)
  (setq helm-autoresize-max-height 70
        helm-autoresize-min-height 20)
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-split-window-in-side-p t))

(use-package helm-descbinds
  :ensure t
  :after helm
  :bind ("C-h b" . helm-descbinds)
  :init
  (fset 'describe-bindings 'helm-descbinds))

(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :config (helm-projectile-on))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind ("C-c h o" . helm-multi-swoop-all))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package material-theme
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

(use-package paredit
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode) . paredit-mode)
  :diminish paredit-mode)

(use-package paren
  :ensure t
  :config
  (show-paren-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-global-mode 1))

(use-package python
  :ensure t
  :config
  (setq python-indent-offset 4))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

;;------------------------------------------------------------------------
;; Packages with external dependencies
;;------------------------------------------------------------------------

(when (executable-find "ag")
  (use-package helm-ag
    :ensure t
    :commands (helm-ag helm-projectile-ag)
    :config
    (setq helm-ag-insert-at-point 'symbol
	  helm-ag-command-option "--path-to-ignore ~/.agignore")))

(when (executable-find "global")
  (use-package helm-gtags
    :ensure t
    :init
    (setq helm-gtags-ignore-case t
          helm-gtags-auto-update t
          helm-gtags-use-input-at-cursor t
          helm-gtags-pulse-at-cursor t
          helm-gtags-prefix-key "\C-cg"
          helm-gtags-suggested-key-mapping t)
    :hook ((dired-mode
	    eshell-mode
	    c-mode c++-mode
	    java-mode asm-mode) . helm-gtags)
    :bind (:map helm-gtags-mode-map
		("M-." . helm-gtags-dwim)
		("M-," . helm-gtags-pop-stack))))

(when (executable-find "sbcl")
  (use-package slime
    :ensure t
    :commands slime
    :init
    (setq inferior-lisp-program "sbcl"
          slime-contribs '(slime-fancy))
    :config
    (load (expand-file-name "~/quicklisp/slime-helper.el"))))

;;------------------------------------------------------------------------
;; Useful little functions
;;------------------------------------------------------------------------

(eval-when-compile
  (defun byte-compile-init-dir ()
    "Byte-compile all your dotfiles."
    (interactive)
    (byte-recompile-directory user-emacs-directory nil 'force)))

;;------------------------------------------------------------------------
;; Emacs custom file
;;------------------------------------------------------------------------

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(when (file-exists-p custom-file)
  (load custom-file))
