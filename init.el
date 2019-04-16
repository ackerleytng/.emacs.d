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
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
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

;; Backup stuff
(setq backup-directory-alist `(("." . ,backup-directory))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5)

(setq-default indent-tabs-mode nil)
(setq-default fill-column 79)

(fset `yes-or-no-p `y-or-n-p)

;;------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package aggressive-indent
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) . aggressive-indent-mode)
  :diminish aggressive-indent-mode)

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char)
         ("M-g g" . avy-goto-line)
	 ("M-g M-g" . avy-goto-line)
	 ("C-'" . avy-goto-word-0)))

(use-package cider
  :ensure t
  :pin melpa-stable)

(use-package clojure-mode
  :ensure t
  :mode (("\\.edn$" . clojure-mode)))

(use-package company
  :ensure t
  :defer t
  :config
  (global-company-mode)
  (setq company-idle-delay 0.15
	company-tooltip-limit 20
	company-echo-delay 0
	company-begin-commands '(self-insert-command)
	company-show-numbers t)
  :diminish company-mode)

(use-package company-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-restclient
  :ensure t
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2))

(use-package elpy
  :ensure t
  :commands (elpy-enable)
  :init (with-eval-after-load 'python (elpy-enable)))

(use-package erc
  :ensure t
  :commands (erc erc-tls))

(use-package exec-path-from-shell
  :if is-mac
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package graphql-mode
  :ensure t
  :mode (("\\.graphql$" . graphql-mode)))

(use-package geiser
  :ensure t)

(use-package helm
  :ensure t
  :defer t
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-z"   . helm-select-action))
  :config
  (helm-autoresize-mode t)
  (setq helm-autoresize-max-height 70
        helm-autoresize-min-height 20)
  (setq helm-M-x-fuzzy-match t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-split-window-in-side-p t))

(use-package helm-ag
  :if (executable-find "ag")
  :ensure t
  :commands (helm-ag helm-projectile-ag)
  :config
  (setq helm-ag-insert-at-point 'symbol
	helm-ag-command-option "--path-to-ignore ~/.agignore"))

(use-package helm-descbinds
  :ensure t
  :after helm
  :bind ("C-h b" . helm-descbinds)
  :config
  (fset 'describe-bindings 'helm-descbinds))

(use-package helm-gtags
  :ensure t
  :if (executable-find "global")
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-suggested-key-mapping t)
  :hook ((dired-mode
	  eshell-mode
	  c-mode c++-mode
	  java-mode asm-mode) . helm-gtags-mode)
  :bind (:map helm-gtags-mode-map
	      ("M-." . helm-gtags-dwim)
	      ("M-," . helm-gtags-pop-stack)))

(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :config (helm-projectile-on))

(use-package helm-swoop
  :ensure t
  :after helm
  :bind ("C-c h o" . helm-multi-swoop-all))

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.js.erb\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (setq js2-basic-offset 2
        js-switch-indent-offset 2
        js2-indent-switch-body t
        js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override nil))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package material-theme
  :ensure t
  :config
  (load-theme 'material t))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

(use-package paredit
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode cider-repl-mode scheme-mode) . paredit-mode)
  :diminish paredit-mode)

(use-package paxedit
  :ensure t
  :after (paredit)
  :hook ((lisp-mode emacs-lisp-mode clojure-mode cider-repl-mode scheme-mode) . paxedit-mode)
  :diminish paxedit-mode
  :bind (("M-<right>" . paxedit-transpose-forward)
         ("M-<left>"  . paxedit-transpose-backward)
         ("M-<up>"    . paxedit-backward-up)
         ("M-<down>"  . paxedit-backward-end)
         ("M-b"       . paxedit-previous-symbol)
         ("M-f"       . paxedit-next-symbol)
         ("C-%"       . paxedit-copy)
         ("C-&"       . paxedit-kill)
         ("C-*"       . paxedit-delete)
         ("C-^"       . paxedit-sexp-raise)
         ("M-u"       . paxedit-symbol-change-case)
         ("C-@"       . paxedit-symbol-copy)
         ("C-#"       . paxedit-symbol-kill)))

(use-package paren
  :ensure t
  :config
  (show-paren-mode 1))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package python
  :ensure t
  :config
  (setq python-indent-offset 4)
  (add-hook 'after-save-hook 'delete-trailing-whitespace))

(use-package pyenv-mode
  :ensure t
  :config
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))

  (add-hook 'projectile-switch-project-hook 'projectile-pyenv-mode-set)
  (add-hook 'python-mode-hook 'pyenv-mode))

(use-package pyenv-mode-auto
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx?\\'"
  :interpreter "node")

(use-package slime
  :ensure t
  :if (executable-find "sbcl")
  :commands slime
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (setq slime-contribs '(slime-fancy))
  (let ((slime-helper-file "~/quicklisp/slime-helper.el"))
    (and (file-exists-p slime-helper-file)
         (load (expand-file-name slime-helper-file)))))

(use-package web-mode
  :ensure t
  :mode ("\\.html$" . web-mode)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq js-indent-level 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-css-colorization t))

(use-package yaml-mode
  :ensure t)

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
