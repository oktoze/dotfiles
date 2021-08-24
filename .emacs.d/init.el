(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(global-hl-line-mode 1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq-default indent-tabs-mode nil)

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

;; use some doom modules
(load "~/.emacs.d/modules/doom-projects.el")
(load "~/.emacs.d/modules/doom-buffers.el")
(load "~/.emacs.d/modules/doom-files.el")

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

(use-package counsel
  :bind ("M-x" . counsel-M-x)
  :bind ("C-x b" . counsel-switch-buffer)
  :bind ("C-x C-b" . counsel-switch-buffer))

(use-package prescient)
(use-package ivy-prescient
  :after ivy
  :config
  (ivy-prescient-mode 1))
(use-package company-prescient
  :after company
  :config
  (company-prescient-mode 1))

(use-package all-the-icons-ivy)

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-done)
         ("C-" . ivy-done)
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
  (load-theme 'doom-dracula))

(use-package avy)

(use-package undo-tree
  :init
  (global-undo-tree-mode 1))

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
  (evil-set-initial-state 'dashboard-mode 'normal)
  (define-key evil-normal-state-map (kbd "C-e") 'end-of-line)
  (define-key evil-visual-state-map (kbd "C-e") 'end-of-line))

(use-package evil-collection
  :after evil
  :init
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :after evil
  :init
  (evil-commentary-mode))
  
(use-package magit
  :init
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
  (magit-status)
  (kz/enable-venv-from-pyrightconfig)
  (projectile-find-file))

(use-package projectile
  :demand t
  :config (projectile-mode)
  :init
  (setq projectile-switch-project-action #'kz/projectile-switch-project-action))

(defun kz/open-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun kz/evil-surround-word ()
  (interactive)
  (execute-kbd-macro (concat "viwS" (char-to-string (read-char)))))

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
    "ff" 'counsel-find-file
	"fR" 'doom/move-this-file
	"fD" 'doom/delete-this-file
    "pf" 'projectile-find-file
    "SPC" 'projectile-find-file
    "pa" 'projectile-add-known-project
    "pp" 'projectile-switch-project
    "pd" 'projectile-dired
    "bd" 'kill-current-buffer
    "bb" 'persp-counsel-switch-buffer
	"br" 'revert-buffer
    "gg" 'magit-status
    "wd" 'delete-window
	"cr" 'lsp-rename
	"cd" 'lsp-find-definition
	"cR" 'lsp-find-references 
	"``" 'persp-switch
	"`n" 'persp-next
	"`p" 'persp-prev
	"`d" 'persp-kill
	"/"  'counsel-projectile-grep)

  (general-imap "j"
	(general-key-dispatch 'self-insert-command
	  "k" 'evil-normal-state))

  (general-define-key :keymaps 'evil-normal-state-map "\"" 'kz/evil-surround-word))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.0)
  (setq which-key-idle-secondary-delay 0.05))

(use-package company
  :defer 2
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (global-company-mode t)
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
  :commands lsp
  :config
  :hook (python-mode . lsp))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp))))

(use-package python-black
  :demand t
  :hook (python-mode . python-black-on-save-mode-enable-dwim))


(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package flycheck
  :defer t
  :hook (lsp-mode . flycheck-mode))

(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

(use-package json-mode)
(use-package yaml-mode)
  
(use-package org
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
  :hook (org-mode . org-bullets-mode))

(use-package visual-fill-column
  :defer
  :hook (org-mode lambda ()
                  (setq visual-fill-column-width 100
                        visual-fill-column-center-text t)
                  (visual-fill-column-mode 1)))
  
(use-package tree-sitter
  :hook (prog-mode lambda ()
                     (tree-sitter-mode 1)
					 (tree-sitter-hl-mode 1)))
(use-package tree-sitter-langs)

(use-package pdf-tools
  :defer t
  :init
  (pdf-tools-install))

(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode 1))

(use-package treemacs)

(use-package perspective
  :init
  (persp-mode t))

(use-package dashboard
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-center-content t)
  (setq dashboard-startup-banner "~/Pictures/berserker-small.png")
  (setq dashboard-banner-logo-title "The door to an another level of the Astral world has opened. Let us proceed.")
  (setq dashboard-items '((projects . 5)
						  (agenda . 5)
						  (bookmarks . 5)))
  (setq dashboard-footer-messages '(""))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-init-info ""))

(use-package beacon
  :config
  (beacon-mode 1)
  (setq beacon-color "#3730d9"))

(use-package org-jira)

(load "~/.emacs.d/modules/gcmh.el")
(gcmh-mode 1)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

