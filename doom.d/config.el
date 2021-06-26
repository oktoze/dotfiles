;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Kamyab Taghizadeh"
      user-mail-address "kamyab.zad@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font "Hack 12"
      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-horizon)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; make underlines a part of 'word'
(modify-syntax-entry ?_ "w")

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;;;;;;;;;;;;
;; Package configs ;;
;;;;;;;;;;;;;;;;;;;;;

;; Org-mode config
(use-package org
  :hook (org-mode lambda ()
                  (display-line-numbers-mode 0)
                  (hl-line-mode 0))
  :config
  (setq org-ellipsis " ▼")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-agenda-files '("~/Org/Tasks")))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode))

;; Company config
(use-package company
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1)
  (setq company-async-wait 0.01)
  (setq company-async-timeout 1)
  (setq company-tooltip-limit 5))

;; lsp-mode config
(use-package lsp-mode
  :config
  (setq lsp-file-watch-threshold 2000))

;; python-black config
(use-package python-black
  :demand t
  :after python
  :hook (python-mode . python-black-on-save-mode-enable-dwim))

;; pyvenv config
(use-package pyvenv
  :config
  (add-hook 'projectile-before-switch-project-hook 'enable-venv-from-pyright)
  (add-hook 'treemacs-switch-workspace-hook 'enable-venv-from-pyright))

;; dap-mode config
(use-package dap-mode
  :config
  (require 'dap-python))

;; diff-hl config (highlighting git changes)
(use-package diff-hl
  :hook (prog-mode . diff-hl-mode))

;; visual-fill-column config
(use-package visual-fill-column
  :defer
  :hook (org-mode lambda ()
                  (setq visual-fill-column-width 100
                        visual-fill-column-center-text t)
                  (visual-fill-column-mode 1)))

;; highlight-indent-guides config
(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'character))

;; key-chord config
(use-package key-chord)
(key-chord-mode 1)

(use-package evil-textobj-line)

;; tree-sitter config
(use-package tree-sitter
  :hook (python-mode lambda ()
                     (tree-sitter-mode 1)))
(use-package tree-sitter-hl
  :hook (python-mode lambda ()
                     (tree-sitter-hl-mode 1)))
(use-package tree-sitter-langs)
;; I don't know what's the use for these, yet!
;; (use-package tree-sitter-debug)
;; (use-package tree-sitter-query)

;;;;;;;;;;;;;;;;;;;;;
;;     Hooks       ;;
;;;;;;;;;;;;;;;;;;;;;

;; go autoformatting
(add-hook 'before-save-hook 'gofmt-before-save)

;;;;;;;;;;;;;;;;;;;;;
;;     Keymaps     ;;
;;;;;;;;;;;;;;;;;;;;;

;; Evil keymaps
(key-chord-define evil-insert-state-map "jk" (kbd "<escape>"))
(key-chord-define evil-visual-state-map "jk" (kbd "<escape>"))
(key-chord-define evil-mc-key-map "jk" (kbd "<escape>"))

(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-z") 'evil-numbers/dec-at-pt)

(defun evil-go-to-norm-exec ()
  (interactive)
  (evil-ex "'<,'>norm "))
(defun evil-surround-word ()
  (interactive)
  (execute-kbd-macro (concat "viwS" (char-to-string (read-char)))))

(define-key evil-visual-state-map (kbd "C-o") 'evil-go-to-norm-exec)
(define-key evil-normal-state-map (kbd "\"")  'evil-surround-word)

(defun evil-insert-into-lines ()
  (interactive)
  (evil-ex "'<,'>norm I"))
(defun evil-append-to-lines ()
  (interactive)
  (evil-ex "'<,'>norm A"))

;; insert/append to lines
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
(define-key evil-visual-state-map (kbd "H-i") 'evil-insert-into-lines)
(define-key evil-visual-state-map (kbd "C-a") 'evil-append-to-lines)

;; Avy keymaps
(map! :leader
      :desc "Avy go to word" "a" #'avy-goto-word-0)

(map! :leader
      (:prefix-map ("d" . "Debug")
       :desc "Open dap-hydra" "h" #'dap-hydra
       :desc "Run dap-debug" "d" #'dap-debug-custom
       :desc "Toggle breakpoint" "b" #'dap-breakpoint-toggle
       :desc "Delete all breakpoints" "D" #'dap-breakpoint-delete-all
       :desc "Quit debugging" "q" #'dap-disconnect))

;;;;;;;;;;;;;;;;;;;;;
;;    Functions    ;;
;;;;;;;;;;;;;;;;;;;;;

(defun dap-debug-custom () (interactive)
       (if (file-exists-p (concat (projectile-project-root) "manage.py"))
           (dap-debug (list :type "python"
                            :args '("runserver" "--noreload")
                            :cwd (projectile-project-root)
                            :console "integratedTerminal"
                            :program (concat (projectile-project-root) "manage.py")
                            :request "launch"
                            :name "Django debug"
                            :debugOptions ["DebugStdLib" "ShowReturnValue" "RedirectOutput"]
                            :django t))
         (command-execute 'dap-debug)))

(defun enable-venv-from-pyright ()
  (if (file-exists-p (concat (projectile-project-root) "pyrightconfig.json"))
      (when (cdr (assoc 'venv (json-read-file (concat (projectile-project-root) "pyrightconfig.json"))))
        (pyvenv-workon (cdr (assoc 'venv (json-read-file (concat (projectile-project-root) "pyrightconfig.json"))))))
    (pyvenv-deactivate)))
