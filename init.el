(setq inhibit-startup-message t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 10)
(global-hl-line-mode 1)
(display-line-numbers-mode 1)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(set-face-attribute 'default nil :font "Hack" :height 130) 

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
	     (ivy-mode 1))

(use-package doom-modeline
  :init (doom-modeline-mode 1))
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t)
  (setq doom-themes-enable-italic t)
  (load-theme 'doom-horizon t))

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
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :init
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (evil-surround-mode))
  
(use-package key-chord
  :after evil
  :config
  (key-chord-mode 1)
  (key-chord-define evil-insert-state-map "jk" (kbd "<escape>"))
  (key-chord-define evil-visual-state-map "jk" (kbd "<escape>")))

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
    "pf" 'projectile-find-file
    "SPC" 'projectile-find-file
    "pa" 'projectile-add-known-project
    "pp" 'projectile-switch-project
    "pd" 'projectile-dired
    "bd" 'kill-current-buffer
    "bb" 'counsel-switch-buffer
    "bB" 'counsel-switch-buffer
    "gg" 'magit-status
    "wd" 'delete-window))

(use-package which-key
  :init
  (which-key-mode)
  :config
  (setq which-key-idle-delay 0.0))

(use-package magit
  :config
  (setq magit-display-buffer-function
	(lambda (buffer)
	  (display-buffer buffer '(display-buffer-same-window)))))

(use-package projectile
  :demand t
  :config (projectile-mode)
  :init
  (setq projectile-switch-project-action #'magit-status))

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


(defun kz/enable-venv-from-pyrightconfig ()
  (interactive)
  (if (file-exists-p (concat (projectile-project-root) "pyrightconfig.json"))
      (when (cdr (assoc 'venv (json-read-file (concat (projectile-project-root) "pyrightconfig.json"))))
        (pyvenv-workon (cdr (assoc 'venv (json-read-file (concat (projectile-project-root) "pyrightconfig.json"))))))
    (pyvenv-deactivate)))

(use-package pyvenv
  :config
  (add-hook 'projectile-before-switch-project-hook 'kz/enable-venv-from-pyrightconfig)
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
  :hook (python-mode lambda ()
                     (tree-sitter-mode 1)
					 (tree-sitter-hl-mode 1)))
(use-package tree-sitter-langs)

(use-package pdf-tools
  :defer t
  :config
  (pdf-tools-install))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("8621edcbfcf57e760b44950bb1787a444e03992cb5a32d0d9aec212ea1cd5234" default))
 '(make-backup-files nil)
 '(package-selected-packages
   '(evil-surround pdf-tools tree-sitter-langs tree-sitter-hl tree-sitter python-black pyvenv visual-fill-column org-mode org-bullets evil-magit yaml-mode json-mode flycheck lsp-pyright lsp-mode company-box company counsel-projectile projectile magit perspective avy which-key general key-chord doom-themes doom-modeline counsel swiper ivy use-package))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
