;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stolen bits from Doom Emacs to make the startup faster ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; supressing garabge-collector at startup
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1)))

;; unset file-name-handler-alist temporarily 
(defvar doom--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
  (lambda ()
    (setq file-name-handler-alist doom--file-name-handler-alist)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(global-hl-line-mode 1)
(global-visual-line-mode 1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq-default indent-tabs-mode nil)
(setf dired-kill-when-opening-new-dired-buffer t)
(set-frame-parameter nil 'alpha-background 90)
(setq scroll-conservatively 1)

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(load-file "~/.emacs.d/elisp/utils.el")

;; Get rid of annoying backup/autosave/lock files
(setq create-lockfiles nil)
(setq temporary-file-directory "~/.emacs.d/tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
(setq custom-file "~/.emacs-custom.el")
(load custom-file)

(set-face-attribute 'default nil :font "JetBrains Mono" :height 130)
(modify-syntax-entry ?_ "w")

;; font for arabic characters
(set-fontset-font
   "fontset-default"
   (cons (decode-char 'ucs #x0600) (decode-char 'ucs #x06ff)) ; arabic
   "Vazir Code")

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

(use-package swiper)

(use-package projectile
  :demand t
  :config (projectile-mode)
  :init
  (setq projectile-switch-project-action #'kz/projectile-switch-project-action))

(use-package counsel
  :bind ("M-x" . counsel-M-x)
  :bind ("C-x b" . counsel-switch-buffer)
  :bind ("C-x C-b" . counsel-switch-buffer))

(use-package prescient
  :config
  (prescient-persist-mode 1))
(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode 1))
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package all-the-icons-ivy
  :config
  (all-the-icons-ivy-setup))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k"  . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)) 
  :config
  (ivy-mode 1)
  (add-to-list 'ivy-initial-inputs-alist '(counsel-M-x . "")))

(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (column-number-mode 1)
  (load-theme 'doom-xcode t))

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-color "#3730d9"))

(use-package highlight-indent-guides
  :defer t
  :config
  (setq highlight-indent-guides-method 'character))

(use-package ace-window
  :defer t
  :config
  (setq aw-dispatch-always t))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/Pictures/berserker.png")
  (setq dashboard-banner-logo-title "The door to an another level of the Astral world has opened. Let us proceed.")
  (setq dashboard-items '((projects . 5)
						  (agenda . 5)
						  (bookmarks . 5)))
  (setq dashboard-footer-messages '(""))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t))


(use-package avy
  :defer t)

(use-package undo-tree
  :init
  (global-undo-tree-mode 1)
  :config
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/tmp/undo-tree/"))))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-C-o-jump t)
  (setq evil-want-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-tree)
  :config
  (evil-mode 1)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  :config
  (evil-collection-init)
  :hook 'dired-mode lambda ()
    (evil-collection-define-key 'normal 'dired-mode-map
      (kbd "SPC") nil))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))


(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode))

(use-package magit
  :defer t
  :config
  (setq magit-display-buffer-function
	(lambda (buffer)
	  (display-buffer buffer '(display-buffer-same-window))))
  (with-eval-after-load 'magit-status
	(define-key magit-status-mode-map (kbd "SPC") nil)))

(defun kz/enable-venv-from-pyrightconfig ()
  (interactive)
  (if (file-exists-p (concat (projectile-project-root) "pyrightconfig.json"))
      (when (cdr (assoc 'venv (json-read-file (concat (projectile-project-root) "pyrightconfig.json"))))
		 (pyvenv-workon (cdr (assoc 'venv (json-read-file (concat (projectile-project-root) "pyrightconfig.json"))))))
    (pyvenv-deactivate)))

(defun kz/projectile-switch-project-action ()
  (persp-switch (projectile-project-name))
  (when (vc-root-dir) (magit-status))
  (kz/enable-venv-from-pyrightconfig)
  (projectile-find-file))

(defun kz/open-emacs-config ()
  (interactive)
  (persp-switch "main")
  (find-file "~/.emacs.d/init.el"))

(defun kz/evil-surround-word ()
  (interactive)
  (execute-kbd-macro (concat "viwS" (char-to-string (read-char)))))

(defun kz/search-project ()
  (interactive)
  (counsel-git-grep))

(use-package general
  :config
  (general-evil-setup t)
  (general-create-definer kz/leader-key
    :keymaps '(normal visual emacs)
    :prefix "SPC")

  (kz/leader-key
    "TAB" 'mode-line-other-buffer
    "a" 'avy-goto-word-0
    "fp" 'kz/open-emacs-config
    "hk" 'describe-key
    "hf" 'describe-function
    "hv" 'describe-variable
    "fs" 'save-buffer
    "fR" 'kz/rename-visiting-file
    "fD" 'kz/delete-visiting-file
    "ff" 'counsel-find-file
    "pf" 'projectile-find-file
    "SPC" 'projectile-find-file
    "pa" 'projectile-add-known-project
    "pp" 'projectile-switch-project
    "pd" 'projectile-dired
    "pb" 'counsel-projectile-switch-to-buffer
    "bd" 'kill-current-buffer
    "bb" 'persp-counsel-switch-buffer
    "bB" 'switch-to-buffer
    "br" 'revert-buffer
    "gg" 'magit-status
    "gB" 'magit-blame-addition
    "gff" 'magit-find-file
    ;; "w"  'ace-window
    "wh" 'evil-window-left
    "wl" 'evil-window-right
    "wj" 'evil-window-down
    "wk" 'evil-window-up
    "wd" 'evil-window-delete
    "wv" 'evil-window-vsplit
    "ws" 'evil-window-split
    "ww" 'ace-window
    "cr" 'lsp-rename
    "cd" 'lsp-find-definition
    "cR" 'lsp-find-references 
    "cf" 'kz/format
    "``" 'kz/persp-switch-by-number
    "`n" 'persp-next
    "`p" 'persp-prev
    "`d" 'persp-kill
    "/"  'kz/search-project)
  (general-imap "j"
	(general-key-dispatch 'self-insert-command
	  "k" 'evil-normal-state))

  (general-define-key
   :keymaps 'evil-normal-state-map
   "\"" 'kz/evil-surround-word))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.0)
  (setq which-key-idle-secondary-delay 0.05))

(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (global-company-mode 1)
  (company-dabbrev-downcase 0)
  (company-async-wait 0.01)
  (company-async-timeout 1)
  (company-tooltip-limit 5))

(use-package company-box
  :after company
  :hook (company-mode . company-box-mode)
  :config
  (setq company-backends '((company-capf company-files))))

(use-package lsp-mode
  :defer t
  :hook ((python-mode go-mode lua-mode) . lsp)
  :commands lsp)

(use-package lsp-ui
  :defer t
  :hook (lsp-mode . lsp-ui-mode))

(use-package lsp-pyright
  :defer t
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright))))

(use-package lua-mode)

(use-package go-mode)

(use-package elpy
  :defer t)

(use-package python-black
  :defer t
  :demand t)

(use-package pyvenv
  :defer t
  :hook (python-mode . pyvenv-mode))

(defun kz/python-settings ()
  (python-black-on-save-mode-enable-dwim))

(add-hook 'python-mode-hook 'kz/python-settings)

(defun kz/go-settings ()
  (yas-minor-mode)
  (setq gofmt-command "goimports")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))
(add-hook 'go-mode-hook 'kz/go-settings)

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package diff-hl
  :defer t
  :hook (prog-mode . diff-hl-mode))

(use-package json-mode
  :defer t)
(use-package yaml-mode
  :defer t)
 
(use-package org
  :defer t
  :hook (org-mode lambda ()
				  (display-line-numbers-mode 0)
				  (hl-line-mode 0))
  :config
  (setq org-directory "~/Org/")
  (setq org-ellipsis " ▼")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN_PROGRESS(p)" "REPEAT(r)" "|" "DONE(d)" "CANCELED(c)")))
  (setq org-tag-alist '(("PERSONAL" . ?p) ("CODING" . ?c) ("PROJECTS" . ?h) ("EDUCATIONAL" . ?u)
        ("ENTERTAINMENT" . ?e) ("BOOKS" . ?b) ("MOVIES" . ?m) ("COURSES" . ?r) ("SKILLS" . ?s))))


(use-package org-bullets
  :defer t
  :hook (org-mode . org-bullets-mode))

(use-package latex-preview-pane
  :defer t)

(use-package visual-fill-column
  :hook (org-mode lambda ()
                  (setq visual-fill-column-width 100
                        visual-fill-column-center-text t)
                  (visual-fill-column-mode 1)))
  
(use-package tree-sitter-langs
  :defer t)

(use-package tree-sitter
  :defer t
  :hook (python-mode lambda ()
                     (require 'tree-sitter-langs)
                     (tree-sitter-mode 1)
                     (tree-sitter-hl-mode 1)))

(use-package pdf-tools
  :defer t
  :init
  (pdf-tools-install))

(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode 1))

(use-package perspective
  :defer t
  :init
  (persp-mode t)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :config
  (setq persp-sort 'created))

(use-package docker
  :ensure t)

(require 'reformatter)
(reformatter-define lua-format
  :program "lua-format"
  :args (kz/lua-format--make-args)
  :lighter " LuaFMT")
(defun kz/lua-format--make-args ()
    (let ((format-file (concat (projectile-project-root) ".lua-format")))
      (if (file-exists-p format-file) '(concat "-i --config=" format-file)
        '("-i"))))

(defun kz/format()
  (interactive)
  (when (eq major-mode 'python-mode)
    (if (eq evil-state 'visual)
        (python-black-region evil-visual-beginning evil-visual-end)
      (python-black-buffer)))
  (when (eq major-mode 'lua-mode) (lua-format-buffer)))

(defun kz/persp-switch-by-number ()
  (interactive)
  (persp-switch-by-number (string-to-number (char-to-string (read-char "Switch to persp number:")))))

(load-file "~/.emacs.d/elisp/overrides.el")
