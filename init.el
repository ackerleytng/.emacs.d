;;------------------------------------------------------------------------
;; Get info about environment
;;------------------------------------------------------------------------

(defconst is-mac (eq system-type 'darwin))
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
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))

(when is-mac
  (setq mac-option-modifier 'meta)
  (setq frame-resize-pixelwise t))

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
                   gc-cons-threshold 100000000
                   gc-cons-percentage 0.1)
             (unless (version<= emacs-version "27.0")
               (setq read-process-output-max (* 1024 1024)))
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

;; Backup stuff
(setq backup-directory-alist `(("." . ,backup-directory))
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 20
      kept-old-versions 5
      ring-bell-function 'ignore)

(setq-default indent-tabs-mode nil)
(setq-default fill-column 79)

(fset `yes-or-no-p `y-or-n-p)

(add-to-list 'default-frame-alist '(font . "SauceCodePro Nerd Font-10"))

;;------------------------------------------------------------------------
;; Packages
;;------------------------------------------------------------------------

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
        aw-scope 'frame))

(use-package add-node-modules-path
  :ensure t
  :hook (((js2-mode rjsx-mode typescript-mode) . add-node-modules-path)))

(use-package ag
  :ensure t)

(use-package aggressive-indent
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode scheme-mode) .
         aggressive-indent-mode)
  :diminish aggressive-indent-mode)

(use-package all-the-icons
  :ensure t)

(use-package avy
  :ensure t
  :bind (("C-;" . avy-goto-char)
         ("M-g g" . avy-goto-line)
	 ("M-g M-g" . avy-goto-line)
	 ("C-'" . avy-goto-word-0)))

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t
  :after (flycheck-clj-kondo)
  :mode (("\\.edn$" . clojure-mode))
  :config
  (require 'flycheck-clj-kondo))

(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; C-c s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package css-mode
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2))

(use-package djvu
  :if (executable-find "djvused")
  :ensure t)

(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  (setq read-extended-command-predicate
        #'command-completion-default-include-p)

  (setq enable-recursive-minibuffers t))

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package erc
  :ensure t
  :commands (erc erc-tls))

(use-package exec-path-from-shell
  :ensure t
  :init
  (exec-path-from-shell-initialize)
  :config
  (setq exec-path-from-shell-arguments nil))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package flycheck-clj-kondo
  :ensure t)

(use-package geiser
  :ensure t)

(use-package go-mode
  :ensure t
  :config
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  :hook (go-mode . lsp-go-install-save-hooks))

(use-package google-c-style
  :ensure t
  :hook (c-mode-common . google-set-c-style))

(use-package graphql-mode
  :ensure t
  :mode (("\\.graphql$" . graphql-mode)))

(use-package graphviz-dot-mode
  :ensure t
  :config
  (setq graphviz-dot-indent-width 4
        graphviz-dot-preview-extension "svg"))

(use-package groovy-mode
  :ensure t
  :mode (("\\.groovy$" . groovy-mode)
         ("\\.gradle$" . groovy-mode)))

(use-package htmlize
  :ensure t)

(use-package js2-mode
  :ensure t
  :mode "\\.js\\'")

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)))

(use-package lua-mode
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package marginalia
  :ensure t
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package modus-themes
  :ensure t
  :init
  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi))

(use-package multiple-cursors
  :ensure t
  :bind (("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)))

(use-package nginx-mode
  :ensure t)

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

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

(use-package package-utils
  :ensure t)

(use-package paredit
  :ensure t
  :hook ((lisp-mode emacs-lisp-mode clojure-mode cider-repl-mode scheme-mode) .
         paredit-mode)
  :diminish paredit-mode
  :bind (:map paredit-mode-map
              ("M-s" . nil)
              ("M-?" . nil)))

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

(use-package python
  :ensure nil
  :config
  (setq python-indent-offset 4)
  :hook
  (after-save . delete-trailing-whitespace))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package recentf
  :ensure t
  :config
  (setq recentf-max-saved-items 512
        recentf-max-menu-items 16
        recentf-auto-cleanup 'never)
  (recentf-mode +1))

(use-package rego-mode
  :ensure t)

(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package rjsx-mode
  :ensure t
  :mode "\\.[jt]sx\\'"
  :config
  (setq js2-basic-offset 2
        js-indent-level 2
        js-switch-indent-offset 2
        js2-strict-missing-semi-warning nil
        js2-missing-semi-one-line-override nil))

(use-package rustic
  :ensure t
  :config
  (setq rustic-format-on-save t))

(use-package savehist
  :init
  (savehist-mode))

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

(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'"
         "\\.tsx\\'")
  :config
  (setq typescript-indent-switch-clauses nil))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  (setq vertico-cycle t))

(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
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

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init (which-key-mode)
  :config
  (setq which-key-idle-delay 0.05))

(use-package yaml-mode
  :ensure t)

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode t)
  :hook (go-mode . yas-minor-mode))

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
