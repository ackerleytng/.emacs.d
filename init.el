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
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))

(use-package aggressive-indent
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) .
         aggressive-indent-mode)
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
  :config
  (global-company-mode)
  (setq
   company-minimum-prefix-length 1
   company-idle-delay 0.1
   company-tooltip-limit 20
   company-begin-commands '(self-insert-command)
   company-show-numbers t)
  :diminish company-mode)

(use-package company-c-headers
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-c-headers))

(use-package company-lsp
  :ensure t
  :after (company lsp-mode)
  :config
  (add-to-list 'company-backends 'company-lsp))

(use-package company-restclient
  :ensure t
  :after (company restclient)
  :config
  (add-to-list 'company-backends 'company-restclient))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2))

(use-package dap-mode
  :ensure t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package djvu
  :if (executable-find "djvused")
  :ensure t)

(use-package elpy
  :ensure t
  :commands elpy-enable
  :hook ((python-mode . elpy-enable)
         (python-mode . elpy-mode))
  :config
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules)))

(use-package erc
  :ensure t
  :commands (erc erc-tls))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package geiser
  :ensure t)

(use-package go-mode
  :ensure t)

(use-package graphql-mode
  :ensure t
  :mode (("\\.graphql$" . graphql-mode)))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4
        graphviz-dot-preview-extension "svg")
  :init
  (use-package company-graphviz-dot))

(use-package groovy-mode
  :ensure t
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gradle$" . groovy-mode)))

(use-package helm
  :ensure t
  :bind (("M-x"     . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("M-y"     . helm-show-kill-ring)
         ("C-x b"   . helm-mini)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-z"   . helm-select-action))
  :config
  (setq
   completion-styles (if (version<= emacs-version "27.0")
                         '(helm-flex) '(flex))
   helm-split-window-inside-p t))

(use-package helm-ag
  :if (executable-find "ag")
  :ensure t
  :after helm
  :commands (helm-ag helm-projectile-ag)
  :config
  (setq helm-ag-insert-at-point 'symbol
        helm-ag-use-agignore t))

(use-package helm-descbinds
  :ensure t
  :after helm
  :bind ("C-h b" . helm-descbinds)
  :config
  (fset 'describe-bindings 'helm-descbinds))

(use-package helm-gtags
  :if (executable-find "global")
  :ensure t
  :after helm
  :config
  (setq helm-gtags-ignore-case t
        helm-gtags-auto-update t
        helm-gtags-use-input-at-cursor t
        helm-gtags-pulse-at-cursor t
        helm-gtags-suggested-key-mapping t)
  :hook ((dired-mode
	  eshell-mode
	  c-mode
	  java-mode asm-mode) . helm-gtags-mode)
  :bind (:map helm-gtags-mode-map
	      ("M-." . helm-gtags-dwim)
	      ("M-," . helm-gtags-pop-stack)))

(use-package helm-lsp
  :ensure t
  :after (helm lsp-mode)
  :commands helm-lsp-workspace-symbol)

(use-package helm-projectile
  :ensure t
  :after (projectile helm)
  :config (helm-projectile-on))

(use-package helm-rg
  :if (executable-find "rg")
  :ensure t
  :after helm)

(use-package helm-swoop
  :ensure t
  :after helm
  :bind ("C-c h o" . helm-multi-swoop-all))

(use-package htmlize
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode (("\\.js\\'" . js2-mode)
         ("\\.js.erb\\'" . js2-mode)
         ("\\.jsx\\'" . js2-jsx-mode))
  :config
  (setq js2-basic-offset 2
        js-switch-indent-offset 2
        js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override nil))

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

(use-package lsp-java
  :ensure t
  :after lsp-mode)

(use-package lsp-mode
  :ensure t
  :commands lsp
  :hook ((scala-mode . lsp)
         (java-mode . lsp)
         (c++-mode . lsp)
         (lsp-mode . lsp-lens-mode))
  :config
  (setq lsp-log-io nil
        lsp-print-performance nil
        lsp-keymap-prefix "C-c l"
        gc-cons-threshold 100000000)
  (unless (version<= emacs-version "27.0")
    (setq read-process-output-max (* 1024 1024))))

(use-package lsp-treemacs
  :ensure t)

(use-package lsp-ui
  :ensure t
  :after lsp-mode
  :config
  (setq lsp-ui-doc-enable nil))

(use-package lua-mode
  :ensure t)

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

(use-package nginx-mode
  :ensure t)

(use-package org
  :ensure nil
  :config
  (setq org-src-preserve-indentation t
        org-src-tab-acts-natively t)
  (plist-put org-format-latex-options :scale 2))

(use-package ox-reveal
  :ensure t
  :config
  (defun toggle-org-html-export-on-save ()
    (interactive)
    (if (memq 'org-reveal-export-to-html after-save-hook)
        (progn
          (remove-hook 'after-save-hook 'org-reveal-export-to-html t)
          (message "Disabled org html export on save for current buffer..."))
      (add-hook 'after-save-hook 'org-reveal-export-to-html nil t)
      (message "Enabled org html export on save for current buffer..."))))

(use-package paredit
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode cider-repl-mode scheme-mode) .
         paredit-mode)
  :diminish paredit-mode)

(use-package paxedit
  :ensure t
  :after (paredit)
  :hook ((lisp-mode emacs-lisp-mode clojure-mode cider-repl-mode scheme-mode) .
         paxedit-mode)
  :diminish paxedit-mode
  :bind (("M-<right>" . paxedit-transpose-forward)
         ("M-<left>"  . paxedit-transpose-backward)
         ("M-<up>"    . paxedit-backward-up)
         ("M-<down>"  . paxedit-backward-end)
         ("C-%"       . paxedit-copy)
         ("C-&"       . paxedit-kill)
         ("C-*"       . paxedit-delete)
         ("C-^"       . paxedit-sexp-raise)
         ("C-@"       . paxedit-symbol-copy)
         ("C-#"       . paxedit-symbol-kill)))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode 1))

(use-package posframe
  :ensure t)

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  (setq projectile-enable-caching t)
  :bind-keymap
  ("C-c p" . projectile-command-map))

(use-package posframe
  :ensure t)

(use-package python
  :ensure nil
  :config
  (setq python-indent-offset 4)
  :hook
  (after-save . delete-trailing-whitespace))

(use-package pyenv-mode
  :ensure t
  :config
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset))))
  :hook ((projectile-switch-project . projectile-pyenv-mode-set)
         (python-mode . pyenv-mode)))

(use-package pyenv-mode-auto
  :ensure t)

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rego-mode
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package rjsx-mode
  :ensure t
  :mode "\\.jsx?\\'"
  :interpreter "node")

(use-package sbt-mode
  :ensure t
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
  ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
  (setq sbt:program-options '("-Dsbt.supershell=false")))

(use-package scala-mode
  :ensure t
  :mode "\\.s\\(c\\|cala\\|bt\\)$")

(use-package slime
  :if (executable-find "sbcl")
  :ensure t
  :commands slime
  :init
  (setq inferior-lisp-program "sbcl")
  :config
  (setq slime-contribs '(slime-fancy slime-asdf))
  (let ((slime-helper-file "~/.quicklisp/slime-helper.el"))
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

(use-package wgrep-ag
  :if (executable-find "ag")
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init
  (yas-global-mode t))

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config (yasnippet-snippets-initialize))

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
