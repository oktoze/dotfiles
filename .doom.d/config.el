;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "JetBrains Mono" :size 16 :weight 'medium)
     doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
(setq doom-theme 'doom-xcode)
(setq display-line-numbers-type t)
(setq org-directory "~/org/")
(setq org-roam-directory "~/org/roam")

;; make underlines a part of 'word'
(modify-syntax-entry ?_ "w")

;; prefer vertical split
(setq split-width-threshold nil)

;; dired config
(put 'dired-find-alternate-file 'disabled nil)
(defun kz/alternate-dired-up-directory ()
  (interactive)
  (find-alternate-file ".."))
(after! evil-collection
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'kz/alternate-dired-up-directory
    "L" 'dired-find-alternate-file))

;; Org-mode config
(after! org
  (setq org-ellipsis " ▼")
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-tags-column -150)
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "IN_PROGRESS(p)" "REPEAT(r)" "|" "DONE(d)" "CANCELED(c)"))))

(defun kz/org-mode ()
  (display-line-numbers-mode 0)
  (visual-fill-column-mode 1)
  (hl-line-mode 0)
  (org-bullets-mode 1)
  (setq visual-fill-column-width 150
        visual-fill-column-center-text t))

(add-hook! 'org-mode-hook 'kz/org-mode)


;; lsp config
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-show-with-mouse t)
(setq lsp-ui-doc-delay 0.5)


(after! company
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 0)
  (setq company-async-wait 0.01)
  (setq company-async-timeout 1)
  (setq company-tooltip-limit 5))

(after! marginalia
  (setq marginalia-align 'right))

(defun kz/python-mode ()
  (python-black-on-save-mode-enable-dwim))

(add-hook! 'python-mode-hook 'kz/python-mode)
(add-hook! 'projectile-before-switch-project-hook 'enable-venv-from-pyright)
(add-hook! 'shell-mode-hook '(lambda () (company-mode -1)))
(add-hook! 'eshell-mode-hook '(lambda () (company-mode -1)))

;; general keymaps
(define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-visual-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-motion-state-map (kbd "C-a") 'move-beginning-of-line)
(define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-visual-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-motion-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-normal-state-map (kbd "C-M-d") 'down-list)
(define-key evil-visual-state-map (kbd "C-M-d") 'down-list)

;; Avy keymaps
(map! :leader
      :desc "Avy go to word" "a" #'avy-goto-word-0)


(defun enable-venv-from-pyright ()
  (if (file-exists-p (concat (projectile-project-root) "pyrightconfig.json"))
      (when (cdr (assoc 'venv (json-read-file (concat (projectile-project-root) "pyrightconfig.json"))))
        (pyvenv-workon (cdr (assoc 'venv (json-read-file (concat (projectile-project-root) "pyrightconfig.json"))))))
    (pyvenv-deactivate)))

(defun evil-surround-word ()
  (interactive)
  (execute-kbd-macro (concat "viwS" (char-to-string (read-char)))))

(define-key evil-normal-state-map (kbd "\"")  'evil-surround-word)

(set-frame-parameter nil 'alpha-background 90)

(defun eshell-other-window ()
  "Open a `eshell' in a new window."
  (interactive)
  (let ((buf (eshell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(defun kz/projectile-run-eshell (&optional arg)
  (interactive "P")
  (let ((project (projectile-acquire-root)))
    (projectile-with-default-dir project
      (let ((eshell-buffer-name (projectile-generate-process-name "eshell" arg project)))
        (eshell-other-window)))))

(map! :leader
      :desc "Open project eshell" "p z" #'kz/projectile-run-eshell)
