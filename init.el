;; -*- lexical-binding: t -*-

(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(let ((default-directory  "~/.emacs.d/site-elisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(eval-when-compile (require 'use-package))

(require 'diminish)

(load (expand-file-name "secret" user-emacs-directory))

(require 'general)

(general-evil-setup)

(general-create-definer rockstar-define-leader :prefix "SPC" :keymaps 'normal)

(defun rockstar-whole-line-or-region-indent (prefix)
  "Indent region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-call-with-region 'indent-region prefix))

(general-define-key "C-c o" 'mode-line-other-buffer)

(general-imap "C-c" 'evil-normal-state)
(general-imap :keymaps 'override "C-x" 'ignore)
;; (general-imap :keymaps 'override "M-x" 'ignore)

(setq ring-bell-function 'ignore)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq enable-recursive-minibuffers t)
(setq confirm-kill-processes nil)
(setq confirm-kill-emacs 'y-or-n-p)

(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 100)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 100)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))

(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t)))

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; (setq ns-control-modifier 'meta)
;; (setq ns-option-modifier 'control)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)

(defmacro rockstar-init-mode-indent (variable &rest extras-to-set)
  "Create a function that initializes a mode with a certain
indentation size."
  (let ((base-statements `((setq evil-shift-width ,variable)
                           (setq tab-width ,variable)
                           (setq standard-indent ,variable))))
        (let ((statements (if extras-to-set
                              (append base-statements
                                      (mapcar (lambda (extra)
                                                `(setq ,extra ,variable))
                                              extras-to-set))
                            base-statements)))
          `(lambda () ,@statements))))

(defmacro rockstar-add-word-syntax-entry (entry)
  `(lambda ()
     (modify-syntax-entry ,entry "w")))

(setq rockstar-preferred-elisp-indent 2)

(add-hook 'emacs-lisp-mode-hook (rockstar-init-mode-indent rockstar-preferred-elisp-indent))
;; Emulate Vim word units
(add-hook 'emacs-lisp-mode-hook (rockstar-add-word-syntax-entry ?-))
(add-hook 'emacs-lisp-mode-hook (rockstar-add-word-syntax-entry ?_))
(add-hook 'emacs-lisp-mode-hook (rockstar-add-word-syntax-entry ?/))
(add-hook 'emacs-lisp-mode-hook (rockstar-add-word-syntax-entry ?*))


(setq rockstar-shell-clear-regex "clear\\|cls")

(defun rockstar-shell-clear-next-output (output)
  "Clear the next output from ComInt and remove this hook."
  (remove-hook 'comint-preoutput-filter-functions 'rockstar-shell-clear-next-output)
  (recenter-top-bottom 0) output)

(defun rockstar-shell-clear-listener (input)
  (when (string-match-p rockstar-shell-clear-regex (string-trim input))
    (add-hook 'comint-preoutput-filter-functions 'rockstar-shell-clear-next-output)))

(use-package xterm-color
  :ensure t
  :pin melpa
  :after shell
  :init
  (setq xterm-color-names
        ["#000000"
         "#ff6d67"
         "#59f68d"
         "#f3f89d"
         "#c9a8fa"
         "#ff92d0"
         "#99ecfd"
         "#c7c7c7"])
  (setq xterm-color-names-bright
        ["#676767"
         "#ff6d67"
         "#59f68d"
         "#f3f89d"
         "#c9a8fa"
         "#ff92d0"
         "#99ecfd"
         "#feffff"])
  :config
  (add-hook 'shell-mode-hook
            (lambda ()
              (setq-local comint-output-filter-functions
                    (remove 'ansi-color-process-output comint-output-filter-functions))
              (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t))))

(use-package comint
  :demand t
  :init
  (setq comint-prompt-read-only t)
  (setq comint-scroll-show-maximum-output nil)
  :config
  (add-hook 'comint-preoutput-filter-functions
            (lambda (text)
              (propertize text 'read-only t))))

(use-package shell
  :demand t
  :init
  :config
  (define-key comint-mode-map [tab] 'company-complete)
  (define-key comint-mode-map (kbd "TAB") 'company-complete)
  (general-nmap :keymaps 'comint-mode-map "q" 'quit-window)
  (general-imap :keymaps 'shell-mode-map "C-c" 'comint-interrupt-subjob)
  (general-imap :keymaps 'shell-mode-map "C-n" 'evil-normal-state)
  (add-hook 'comint-mode-hook
            (lambda ()
              (add-hook 'evil-insert-state-exit-hook 'company-abort nil t)
              (setq-local company-active-map
                          (let ((keymap (make-sparse-keymap)))
                            (set-keymap-parent keymap company-active-map)
                            (define-key keymap [tab] 'company-complete-selection)
                            (define-key keymap (kbd "TAB") 'company-complete-selection)
                            (define-key keymap [return] 'rockstar-company-shifted-return)
                            (define-key keymap (kbd "RET") 'rockstar-company-shifted-return) keymap))))
  (add-hook 'shell-mode-hook
            (lambda ()
              (add-hook 'comint-input-filter-functions
                                'rockstar-shell-clear-listener nil t))))

(defvar rockstar-term-shell "/bin/bash")

(defun rockstar-term ()
  (interactive)
  (projectile-run-term rockstar-term-shell))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :pin melpa
    :config
    (exec-path-from-shell-initialize)))

(use-package shackle
  :ensure t
  :pin melpa
  :after (magit shell)
  :init
  (setq helm-display-function 'pop-to-buffer)
  (setq swiper-helm-display-function 'pop-to-buffer)
  (setq shackle-rules '((magit-status-mode :same t :inhibit-window-quit t)
                        ;; (magit-log-mode :same t :inhibit-window-quit t)
                        ;; (magit-diff-mode :same t :inhibit-window-quit t)
                        ;; (magit-process-mode :same t :inhibit-window-quit t)
                        ("*Help*" :same t :inhibit-window-quit t)
                        ("\\`\\*helm.*?\\*\\'" :regexp t :align t :size 0.33)
                        ("*swiper*" :regexp t :align t :size 0.33)
                        ("*evil-registers*" :regexp t :align t :size 0.33)
                        ("\\`\\*shell.*?\\*\\'" :regexp t :same t :inhibit-window-quit t)))
  :config
  (shackle-mode 1))

(use-package smex
  :ensure t
  :pin melpa
  :config
  (smex-initialize))

(use-package wgrep
  :ensure t
  :pin melpa)

(require 'subr-x)

(defun rockstar-trim-last-ex-history-entry ()
  (let ((cmd (pop evil-ex-history)))
    (push (string-trim cmd) evil-ex-history)))

(defun rockstar-append-ex-command-target (arg)
  (let ((cmd (pop evil-ex-history)))
    (push (concat cmd arg) evil-ex-history)))

(defun rockstar-helm-edit-action (file)
  (evil-edit file)
  (rockstar-append-ex-command-target file))

(defun rockstar-helm-edit ()
  (interactive)
  (let ((helm-find-files-actions
         (cons '("Find file" . rockstar-helm-edit-action)
               (cdr helm-find-files-actions))))
    (helm-find-files nil)
    (rockstar-trim-last-ex-history-entry)))

(defun rockstar-helm-vsplit-action (file)
  (evil-window-vsplit nil file)
  (rockstar-append-ex-command-target file))

(defun rockstar-helm-vsplit ()
  (interactive)
  (let ((helm-find-files-actions
         (cons '("Find file in vertical split" . rockstar-helm-vsplit-action)
               (cdr helm-find-files-actions))))
    (helm-find-files nil)
    (rockstar-trim-last-ex-history-entry)))

(defun rockstar-helm-split-action (file)
  (evil-window-split nil file)
  (rockstar-append-ex-command-target file))

(defun rockstar-helm-split ()
  (interactive)
  (let ((helm-find-files-actions
         (cons '("Find file in split" . rockstar-helm-split-action)
               (cdr helm-find-files-actions))))
    (helm-find-files nil)
    (rockstar-trim-last-ex-history-entry)))

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))

(use-package helm
  :after evil
  :diminish helm-mode
  :init
  (setq helm-mode-handle-completion-in-region t)
  (setq helm-M-x-fuzzy-match t)
  (setq helm-recentf-fuzzy-match t)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-echo-input-in-header-line t)
  (setq helm-ff-file-name-history-use-recentf t)
  :config
  (require 'helm-config)
  ;; https://tuhdo.github.io/helm-intro.html

  (define-key helm-map (kbd "<tab>") #'helm-execute-persistent-action) ; rebind tab to run persistent action
  (define-key helm-map (kbd "C-i") #'helm-execute-persistent-action) ; make TAB work in terminal
  (define-key helm-map (kbd "C-z")  #'helm-select-action) ; list actions using C-z

  (general-define-key :keymaps 'helm-map "<escape>" 'helm-keyboard-quit)
  (general-nmap "M-i" 'helm-imenu)
  (general-nmap "M-p" #'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (general-define-key "M-x" #'helm-M-x)
  (general-define-key "C-h a" #'helm-apropos)
  (general-define-key "C-x b" #'helm-mini)
  (evil-ex-define-cmd "buffers" #'helm-mini)
  ;; TODO: Implement bd[elete]
  (define-key evil-ex-map "e " #'rockstar-helm-edit)
  (define-key evil-ex-map "edit " #'rockstar-helm-edit)
  (define-key evil-ex-map "vs " #'rockstar-helm-vsplit)
  (define-key evil-ex-map "vsplit " #'rockstar-helm-vsplit)
  (define-key evil-ex-map "sp " #'rockstar-helm-split)
  (define-key evil-ex-map "split " #'rockstar-helm-split)

  (helm-mode 1)

  (add-hook 'helm-minibuffer-set-up-hook
            'spacemacs//helm-hide-minibuffer-maybe))

(use-package helm-projectile
  :ensure t
  :pin melpa
  :after (helm projectile)
  :init
  (setq helm-projectile-truncate-lines t)
  (setq projectile-switch-project-action #'helm-projectile)
  :config
  (helm-projectile-on)
  (general-nmap "C-p" 'helm-projectile)
  (general-nmap "C-n" 'ignore))

(use-package helm-ag
  :ensure t
  :pin melpa
  :after helm
  :init
  (setq helm-ag-base-command "rg --no-heading")
  (setq helm-follow-mode-persistent t)
  :config
  (general-nmap "C-S-s" 'helm-do-ag-project-root))

(use-package swiper-helm
  :ensure t
  :pin melpa
  :after helm
  :config
  (general-nmap "C-/" 'swiper-helm))

(use-package flx
  :ensure t
  :pin melpa)

;; (use-package ivy
;;   :ensure t
;;   :pin melpa
;;   :after (smex hydra flx)
;;   :diminish ivy-mode
;;   :init
;;   (setq ivy-use-virtual-buffers t)
;;   (setq ivy-initial-inputs-alist nil)
;;   :config
;;   (ivy-mode 1)
;;   (general-define-key "C-c s" 'swiper)
;;   (general-nmap "C-/" 'swiper)
;;   (general-define-key :keymaps 'ivy-minibuffer-map "<escape>" 'abort-recursive-edit)
;;   (general-define-key :keymaps 'ivy-minibuffer-map "C-j" 'ivy-next-line)
;;   (general-define-key :keymaps 'ivy-minibuffer-map "C-k" 'ivy-previous-line)
;;   (general-define-key :keymaps 'ivy-minibuffer-map "M-j" 'ivy-next-history-element)
;;   (general-define-key :keymaps 'ivy-minibuffer-map "M-k" 'ivy-previous-history-element)
;;   (general-define-key :keymaps 'ivy-minibuffer-map "C-d" 'ivy-alt-done)
;;   (general-define-key :keymaps 'ivy-minibuffer-map "C-M-d" 'ivy-immediate-done)
;;   (general-define-key "<f5>" 'ivy-push-view)
;;   (general-define-key "<f6>" 'ivy-pop-view))

(use-package mwim
  :ensure t
  :pin melpa
  :config
  (general-define-key "C-a" 'mwim-beginning-of-code-or-line)
  (general-define-key "C-e" 'mwim-end-of-code-or-line))

;; (use-package counsel
;;   :ensure t
;;   :pin melpa
;;   :after ivy
;;   :diminish counsel-mode
;;   :config
;;   (counsel-mode 1)
;;   (general-define-key "C-c r" 'counsel-recentf))

(defun rockstar-neotree-toggle ()
  "Toggle NeoTree at the project root if it exists, or the default
directory."
  (interactive)
  (let ((default-dir default-directory)
        (project-dir (condition-case nil (projectile-project-root) (error nil)))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if (neo-global--window-exists-p)
        (progn
          (neotree-dir (or project-dir default-dir))
          (neotree-find file-name)))))

(defun neo-open-file-hide (full-path &optional arg)
  "Open a file node and hides tree."
  (neo-global--select-mru-window arg)
  (find-file full-path)
  (neotree-hide))

(defun neotree-enter-hide (&optional arg)
  "Enters file and hides neotree directly"
  (interactive "P")
  (neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))

(use-package neotree
  :after all-the-icons
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (general-nmap "C-S-b" 'rockstar-neotree-toggle)
  (general-nmap :keymaps 'neotree-mode-map "<escape>" 'neotree-toggle)
  (add-to-list 'all-the-icons-icon-alist
               '("\\.tsx$" all-the-icons-fileicon "tsx" :height 1.0 :v-adjust -0.1 :face all-the-icons-cyan-alt)))

(use-package vscode-icon
  :ensure t
  :pin melpa
  :commands (vscode-icon-for-file))

;; (use-package dired-sidebar
;;   :ensure t
;;   :pin melpa
;;   :after (all-the-icons all-the-icons-dired evil)
;;   :init
;;   ;; https://github.com/jojojames/dired-sidebar#my-settings
;;   (add-hook 'dired-sidebar-mode-hook
;;             (lambda ()
;;               (unless (file-remote-p default-directory)
;;                 (auto-revert-mode))))
;;   (setq dired-sidebar-use-evil-integration nil)
;;   ;; (setq dired-sidebar-theme 'icons)
;;   :config
;;   ;; TODO: Configure RET on directory to toggle subtree
;;   (general-nmap "C-\\" 'dired-sidebar-toggle-sidebar))

(use-package projectile
  :ensure t
  :pin melpa
  ;; :after ivy
  :init
  ;; (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (general-define-key :keymaps 'projectile-mode-map
                      "C-c p"
                      'projectile-command-map)
  (general-nmap "C-S-p" #'projectile-run-shell)
  (projectile-mode 1))

;; (use-package whole-line-or-region
;;   :config
;;   (whole-line-or-region-global-mode t)
;;   (general-define-key "C-M-\\" 'rockstar-whole-line-or-region-indent))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-min-dir-content 1)
  (setq uniquify-ignore-buffers-re "^\\*"))

;; (use-package nyan-mode
;;   :ensure t
;;   :pin melpa
;;   :config
;;   (nyan-mode 1))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(defun rockstar-company-shifted-return ()
  "Abort the current company completion and create a new line."
  (interactive)
  (company-abort)
  (execute-kbd-macro (kbd "<return>")))

(use-package company
  :diminish company-mode
  :init
  (setq company-minimum-prefix-length 1)
  :config
  (global-company-mode 1)
  (general-define-key :keymaps 'company-active-map
                      "<S-return>"
                      'rockstar-company-shifted-return)
  (general-define-key :keymaps 'company-active-map "C-n" 'company-select-next)
  (general-define-key :keymaps 'company-active-map "C-p" 'company-select-previous))

(use-package all-the-icons
  :ensure t
  :pin melpa)

(use-package all-the-icons-dired
  :ensure t
  :pin melpa
  :after all-the-icons)

;; (use-package smart-mode-line
;;   :ensure t
;;   :pin melpa-stable
;;   :init
;;   (setq sml/theme 'dark)
;;   :config
;;   (sml/setup))
(use-package spaceline
  :ensure t
  :pin melpa-stable
  :init
  (setq spaceline-workspace-numbers-unicode nil)
  (setq powerline-image-apple-rgb t)  ; Fix colors on macOS
  (setq powerline-height 20)
  (setq powerline-default-separator nil)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  (spaceline-toggle-window-number-off)
  (spaceline-toggle-workspace-number-on)
  (spaceline-toggle-nyan-cat-on))

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :after evil
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list)
  (general-nmap :keymaps 'flycheck-mode-map "[q" 'flycheck-previous-error)
  (general-nmap :keymaps 'flycheck-mode-map "]q" 'flycheck-next-error))

;; (use-package intero
;;   :after (haskell-mode flycheck)
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode)
;;   (flycheck-add-next-checker 'intero
;;                              '(warning . haskell-hlint)))

(use-package lsp-mode
  :ensure t
  :pin melpa)

(use-package company-lsp
  :ensure t
  :pin melpa
  :after company
  :init
  ;; (setq company-lsp-enable-snippet t)
  :config
  (push 'company-lsp company-backends))

(use-package lsp-ui
  :ensure t
  :pin melpa
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (general-nmap :keymaps 'lsp-ui-mode-map "<f2>" 'lsp-rename)
  (general-nmap :keymaps 'lsp-ui-mode-map "<f3>" 'lsp-format-buffer))

(use-package lsp-haskell
  :ensure t
  :pin melpa
  :after (lsp-mode lsp-ui)
  :init
  (setq lsp-haskell-process-path-hie "~/.local/bin/hie-wrapper")
  :config
  (add-hook 'haskell-mode-hook 'lsp-haskell-enable)
  (add-hook 'haskell-mode-hook 'flycheck-mode)
  (add-hook 'haskell-mode-hook (rockstar-init-mode-indent 2)))

(defun rockstar-org-gcal-fetch-silently ()
    (org-gcal-sync nil t t))

(use-package org
  :init
  (setq org-log-done t)
  (setq org-agenda-files (list "~/org/gcal.org" "~/org/planner.org"))
  :config
  (add-hook 'org-agenda-mode-hook #'rockstar-org-gcal-fetch-silently))

(use-package winum
  :ensure t
  :pin melpa-stable
  :init
  (setq winum-auto-setup-mode-line nil)
  :config
  (winum-mode)
  ;; (general-define-key :keymaps 'evil-window-map "0" 'winum-select-window-0)
  ;; (general-define-key :keymaps 'evil-window-map "1" 'winum-select-window-1)
  ;; (general-define-key :keymaps 'evil-window-map "2" 'winum-select-window-2)
  ;; (general-define-key :keymaps 'evil-window-map "3" 'winum-select-window-3)
  ;; (general-define-key :keymaps 'evil-window-map "4" 'winum-select-window-4)
  ;; (general-define-key :keymaps 'evil-window-map "5" 'winum-select-window-5)
  ;; (general-define-key :keymaps 'evil-window-map "6" 'winum-select-window-6)
  ;; (general-define-key :keymaps 'evil-window-map "7" 'winum-select-window-7)
  ;; (general-define-key :keymaps 'evil-window-map "8" 'winum-select-window-8)
  ;; (general-define-key :keymaps 'evil-window-map "9" 'winum-select-window-9))
  )

(use-package bash-completion
  :ensure t
  :pin melpa
  :config
  (bash-completion-setup))

(use-package org-gcal
  :init
  (setq org-gcal-client-id my-gcal-client-id
  org-gcal-client-secret my-gcal-client-secret
  org-gcal-file-alist (list `(,my-gcal-email  . "~/org/gcal.org"))))

(setq inhibit-startup-message t)
;; (setq initial-buffer-choice
;;       (lambda ()
;;         (org-agenda-list)
;;         (let ((agenda-buffer (get-buffer "*Org Agenda*")))
;;           (let ((agenda-window (get-buffer-window agenda-buffer)))
;;             (delete-other-windows agenda-window)
;;             agenda-buffer))))

(use-package atomic-chrome
  :ensure t
  :pin melpa
  :config
  (atomic-chrome-start-server))

(use-package emojify
  :ensure t
  :config
  (add-hook 'org-agenda-mode-hook #'emojify-mode))

(use-package evil
  :ensure t
  :pin melpa
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration nil)
  (setq evil-want-Y-yank-to-eol t)
  (setq evil-cross-lines t)
  :config
  (evil-mode 1)
  (add-hook 'git-commit-mode-hook #'evil-insert-state)
  (general-nmap "M-s" 'evil-window-split)
  (general-nmap "M-v" 'evil-window-vsplit)
  (general-nmap "M-c" 'delete-window)
  (general-nmap "M-j" 'evil-window-down)
  (general-nmap "M-k" 'evil-window-up)
  (general-nmap "M-h" 'evil-window-left)
  (general-nmap "M-l" 'evil-window-right))

(use-package evil-collection
  :ensure t
  :pin melpa
  :after (evil neotree)
  :init
  (setq evil-collection-company-use-tng nil)
  ;; line-mode has some weird effects on how output works, I think we want to be
  ;; more conscious about using it
  (setq evil-collection-term-sync-state-and-mode-p nil)
  :config
  (evil-collection-init)
  ;; Still enter character mode on insert
  (add-hook 'term-mode-hook
            (lambda ()
              (setq bidi-paragraph-direction 'left-to-right)
              (add-hook 'evil-insert-state-entry-hook
                        evil-collection-term-sync-state-function nil t)))
  (general-nmap :keymaps 'neotree-mode-map "<return>" 'neotree-enter-hide)
  ;; (add-hook
  ;;  'neotree-mode-hook
  ;;  (lambda ()
  ;;    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter-hide)))
  ;; TODO: Does this actually work? 
  (general-def 'normal term-mode-map
    [remap undo-tree-undo] #'ignore
    [remap undo-tree-redo] #'ignore)
  (general-imap :keymaps 'term-mode-map "C-c" 'term-kill-subjob)
  (general-nmap :keymaps 'term-mode-map "q" 'quit-window)

  ;; wgrep
  ;; Unbind these until I decide how I want to do this
  (general-define-key :keymaps 'wgrep-mode-map "ZQ" nil)
  (general-define-key :keymaps 'wgrep-mode-map "ZZ" nil))

;; (use-package evil-easymotion
;;   :ensure t
;;   :pin melpa
;;   :after evil
;;   :config
;;   (define-key evil-motion-state-map (kbd ",") nil)
;;   (evilem-default-keybindings ", ,"))

(use-package evil-commentary
  :ensure t
  :pin melpa
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode +1))

(use-package magit
  :ensure t
  :pin melpa
  :config
  ;; These bindings for opening heading levels collide with my own config
  (general-define-key :keymaps 'magit-status-mode-map "M-1" nil)
  (general-define-key :keymaps 'magit-status-mode-map "M-2" nil)
  (general-define-key :keymaps 'magit-status-mode-map "M-3" nil)
  (general-define-key :keymaps 'magit-status-mode-map "M-4" nil)
  (general-define-key :keymaps 'magit-status-mode-map "M-5" nil)
  (general-define-key :keymaps 'magit-diff-mode-map "M-1" nil)
  (general-define-key :keymaps 'magit-diff-mode-map "M-2" nil)
  (general-define-key :keymaps 'magit-diff-mode-map "M-3" nil)
  (general-define-key :keymaps 'magit-diff-mode-map "M-4" nil)
  (general-define-key :keymaps 'magit-diff-mode-map "M-5" nil)
  (general-nmap "C-S-g" 'magit-status)
  (general-nmap "C-M-g" 'magit-file-popup))

(use-package evil-magit
  :ensure t
  :pin melpa)

(use-package yaml-mode
  :ensure t
  :pin melpa
  :config
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
  (add-hook 'yaml-mode-hook 'display-line-numbers-mode)
  (add-hook 'yaml-mode-hook (rockstar-init-mode-indent 2))
  (add-hook 'yaml-mode-hook
            '(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package evil-org
  :ensure t
  :pin melpa
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (general-nmap :keymaps 'evil-org-mode-map "gx" #'org-open-at-point))
  ;; (general-nmap :keymaps 'evil-org-mode-map "C-M-o" (evil-org-define-eol-command org-insert-heading))
  ;; (general-nmap :keymaps 'evil-org-mode-map "M-o" (evil-org-define-eol-command org-meta-return))
  ;; (general-nmap :keymaps 'evil-org-mode-map "o" #'evil-open-below)
  ;; (general-nmap :keymaps 'evil-org-mode-map "O" #'evil-org-open-above))

(use-package restart-emacs
  :ensure t
  :pin melpa)
    

;; https://github.com/Fuco1/smartparens/issues/80
;; https://github.com/xenodium/dotsies/commit/c94ea6116346ab7fe2c0171fa859480dedebb2ed
(defun rockstar-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(use-package smartparens
  :ensure t
  :pin melpa
  :diminish smartparens-mode
  :init
  (setq sp-show-pair-delay 0.2
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  :config
  (require 'smartparens-config)
  (sp-local-pair 'prog-mode
                 "{"
                 nil
                 :post-handlers '((rockstar-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'prog-mode
                 "["
                 nil
                 :post-handlers '((rockstar-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'prog-mode
                 "("
                 nil
                 :post-handlers '((rockstar-create-newline-and-enter-sexp "RET")))
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

(use-package hydra
  :ensure t
  :pin melpa)

;; (use-package ivy-hydra
;;   :after hydra
;;   :ensure t
;;   :pin melpa
;;   :config
;;   (rockstar-define-leader :keymaps 'ivy-minibuffer-map "SPC" 'hydra-ivy/body)
;;   (rockstar-define-leader :keymaps 'ivy-minibuffer-map "j" 'ivy-avy)
  ;; (rockstar-define-leader :keymaps 'ivy-minibuffer-map "u" 'ivy-occur))

(use-package hl-todo
  :ensure t
  :pin melpa
  :config
  (global-hl-todo-mode +1))

;; (use-package counsel-projectile
;;   :after counsel
;;   :ensure t
;;   :pin melpa
;;   :config
;;   (counsel-projectile-mode +1)
;;   (general-nmap "C-p" #'counsel-projectile)
;;   (general-nmap "C-s" #'counsel-projectile-ag)
;;   (general-nmap "C-n" #'counsel-projectile-switch-project)
;;   (general-nmap "C-j" #'counsel-imenu))

(use-package wgrep
  :ensure t
  :pin melpa)

(use-package ag
  :ensure t
  :pin melpa)

(use-package avy
  :ensure t
  :pin melpa
  :init
  (setq avy-timeout-seconds 0.3)
  :config
  ;; Note: Evil integration is provided by evil-collection
  ;; (define-key evil-motion-state-map (kbd ",") nil)
  ;; (general-nmap ", w" 'avy-goto-word-0-below)
  ;; (general-nmap ", b" 'avy-goto-word-0-above)
  ;; (general-nmap ", j" 'avy-goto-line-below)
  ;; (general-nmap ", k" 'avy-goto-line-above)
  (global-set-key (kbd "C-'") 'avy-goto-char-timer)
  (global-set-key (kbd "M-g '") 'avy-goto-line)
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume))

(use-package evil-easymotion
  :ensure t
  :pin melpa
  :after (evil-collection avy)
  :config
  (define-key evil-motion-state-map (kbd ",") nil)
  ;; Requires evil-collection avy integration
  (general-define-key :keymaps 'evilem-map "," 'avy-goto-word-1)
  (evilem-default-keybindings ","))

(use-package eyebrowse
  :ensure t
  :pin melpa
  :init
  (setq eyebrowse-new-workspace t)
  :config
  (general-nmap "M-1" 'eyebrowse-switch-to-window-config-1)
  (general-nmap "M-2" 'eyebrowse-switch-to-window-config-2)
  (general-nmap "M-3" 'eyebrowse-switch-to-window-config-3)
  (general-nmap "M-4" 'eyebrowse-switch-to-window-config-4)
  (general-nmap "M-5" 'eyebrowse-switch-to-window-config-5)
  (general-nmap "M-[" 'eyebrowse-prev-window-config)
  (general-nmap "M-]" 'eyebrowse-next-window-config)
  (eyebrowse-mode t))

(use-package prettier-js
  :ensure t
  :pin melpa
  :after (typescript-mode web-mode)
  :config
  (add-hook 'javascript-mode-hook 'prettier-js-mode)
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

(use-package docker
  :ensure t
  :pin melpa
  :config
  (general-define-key "C-c d" 'docker))

(use-package anzu
  :ensure t
  :pin melpa
  :diminish anzu-mode
  :init
  (setq anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode +1))

(use-package add-node-modules-path
  :ensure t
  :pin melpa)

(use-package evil-anzu
  :ensure t
  :after (evil anzu)
  :pin melpa)

(use-package evil-surround
  :ensure t
  :pin melpa
  :diminish evil-surround-mode
  :config
  (global-evil-surround-mode t))
;; (use-package evil-snipe
;;   :ensure t
;;   :after evil
;;   :pin melpa
;;   :diminish evil-snipe-local-mode
;;   :init
;;   (setq evil-snipe-spillover-scope 'buffer)
;;   :config
;;   (add-hook 'prog-mode-hook 'turn-on-evil-snipe-mode)
;;   (add-hook 'text-mode-hook 'turn-on-evil-snipe-mode))

(setq rockstar-preferred-web-mode-indent 2)

(setq rockstar-preferred-json-indent 2)

(use-package web-mode
  :ensure t
  :pin melpa
  :after (add-node-modules-path smartparens)
  :init
  (setq web-mode-enable-auto-quoting nil)
  (setq web-mode-tag-auto-close-style t)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-indentation t)
  (setq web-mode-enable-auto-opening t)
  :config
  (add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
  ;; Source: http://spacemacs.org/layers/+frameworks/react/README.html
  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))
  ;; Smartparens doesn't integrate that well with defining tags in web-mode
  (sp-with-modes '(web-mode)
    (sp-local-pair "<" nil :actions nil))
  (add-hook 'web-mode-hook
            (rockstar-init-mode-indent rockstar-preferred-web-mode-indent
                                       web-mode-markup-indent-offset
                                       web-mode-code-indent-offset
                                       web-mode-css-indent-offset
                                       web-mode-attr-indent-offset
                                       web-mode-attr-value-indent-offset
                                       typescript-indent-level))
  (add-hook 'javascript-mode-hook
            (rockstar-init-mode-indent rockstar-preferred-web-mode-indent
                                       js-indent-level))
  (add-hook 'javascript-mode-hook (rockstar-add-word-syntax-entry ?_))
  (add-hook 'javascript-mode-hook #'add-node-modules-path)
  (add-hook 'js-jsx-mode-hook
            (rockstar-init-mode-indent rockstar-preferred-web-mode-indent
                                       web-mode-attr-indent-offset
                                       js-indent-level))
  (add-hook 'css-mode-hook
            (rockstar-init-mode-indent rockstar-preferred-web-mode-indent
                                       css-indent-offset
                                       web-mode-css-indent-offset))
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'web-mode-hook (rockstar-add-word-syntax-entry ?_))
  (add-hook 'javascript-mode-hook #'add-node-modules-path)
  (add-hook 'css-mode-hook #'add-node-modules-path)
  (add-hook 'json-mode-hook
            (rockstar-init-mode-indent rockstar-preferred-json-indent)))

(setq rockstar-preferred-ts-indent 2)
(setq rockstar-use-tslint t)

(defun rockstar-setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1)
  (when rockstar-use-tslint
    (add-hook 'after-save-hook 'tslint-fix-file-and-revert nil 'local)))

(defun rockstar-setup-typescript ()
  (interactive)
  (when rockstar-use-tslint
    (add-hook 'after-save-hook 'tslint-fix-file-and-revert nil 'local)))

;; https://gist.github.com/hgiasac/b8e4f50ea9dc504e3e52163e58872a4e
(defun tslint-fix-file ()
  "Tslint fix file."
  (interactive)
  (message (concat "tslint --fix " (buffer-file-name)))
  (call-process "tslint" nil nil nil "--fix" (buffer-file-name)))

(defun tslint-fix-file-and-revert ()
  "Format the current file with TSLint."
  (interactive)
  (when (or (eq major-mode 'typescript-mode) (eq major-mode 'web-mode))
    (if (executable-find "tslint")
        (tslint-fix-file)
      (message "TSLint not found."))))

(use-package typescript-mode
  :ensure t
  :pin melpa
  :config
  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  (add-hook 'typescript-mode-hook (rockstar-add-word-syntax-entry ?_))
  (add-hook 'typescript-mode-hook #'rockstar-setup-typescript)
  (add-hook 'typescript-mode-hook (rockstar-init-mode-indent rockstar-preferred-ts-indent
                                                             typescript-indent-level))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (rockstar-setup-typescript)))))

(defun rockstar-company-transformer-javascript-lsp (candidates)
  (let ((completion-ignore-case t))
    (all-completions (company-grab-symbol) candidates)))

;; https://github.com/emacs-lsp/lsp-javascript
(defun rockstar-fix-company-for-javascript-lsp nil
  (make-local-variable 'company-transformers)
  (push 'rockstar-company-transformer-javascript-lsp company-transformers))

(use-package lsp-javascript-typescript
  :ensure t
  :pin melpa
  :after (typescript-mode web-mode)
  :config
  ;; TODO: Pin version of this package to protect this workaround
  ;; https://github.com/emacs-lsp/lsp-javascript/commit/ab62826962887e82f0bc968817be4fc89a6953e4
  (defun lsp-javascript-typescript--render-string (str)
    (condition-case nil
        (with-temp-buffer
          (delay-mode-hooks (web-mode))
          (insert str)
          (font-lock-ensure)
          (buffer-string))
      (error str)))

  (add-hook 'web-mode-hook #'rockstar-fix-company-for-javascript-lsp)
  (add-hook 'web-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'web-mode-hook #'rockstar-setup-typescript))

(use-package yasnippet
  :diminish yas-minor-mode
  :ensure t
  :pin melpa
  :config
  (yas-global-mode 1))

(defun rockstar-colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))

(defun rockstar-colorize-buffer (proc &rest args)
  (interactive)
  (with-current-buffer (process-buffer proc)
    (read-only-mode -1)
    (ansi-color-apply-on-region (point-min) (point-max))
    (read-only-mode 1)))

(use-package ansi-color
  :ensure t
  :pin melpa
  :after magit
  :config
  (add-hook 'compilation-filter-hook #'rockstar-colorize-compilation-buffer)
  ;; https://github.com/magit/magit/issues/1878
  (advice-add 'magit-process-filter :after #'rockstar-colorize-buffer))

;; (use-package tide
;;   :ensure t
;;   :pin melpa
;;   :after (web-mode company typescript-mode)
;;   :init
;;   (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
;;   :config
;;   (add-hook 'typescript-mode-hook #'add-node-modules-path)
;;   (add-hook 'typescript-mode-hook #'rockstar-setup-tide-mode)
;;   (add-hook 'typescript-mode-hook (rockstar-init-mode-indent rockstar-preferred-ts-indent
;;                                                              typescript-indent-level))
;;   (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
;;   (flycheck-add-mode 'typescript-tslint 'web-mode)
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;                 (setq-local web-mode-attr-indent-offset rockstar-preferred-ts-indent)
;;                 (setq-local web-mode-code-indent-offset rockstar-preferred-ts-indent)
;;                 (setq-local web-mode-attr-value-indent-offset rockstar-preferred-ts-indent)
;;                 (setq-local web-mode-markup-indent-offset rockstar-preferred-ts-indent)
;;                 (setq-local typescript-indent-level rockstar-preferred-ts-indent)
;;                 (rockstar-setup-tide-mode)))))

(use-package hindent
  :ensure t
  :pin melpa
  :config
  (add-hook 'haskell-mode-hook 'hindent-mode))

(use-package feature-mode
  :ensure t
  :pin melpa
  :config
  (add-to-list 'auto-mode-alist '("\.feature$" . feature-mode)))

;; (use-package evil-unimpaired
;;   :config
;;   (evil-unimpaired-define-pair "q" '(flycheck-previous-error . flycheck-next-error))
;;   (evil-unimpaired-define-pair "b" '(xah-previous-user-buffer . xah-next-user-buffer))
;;   (evil-unimpaired-define-pair "a" '(evil-unimpaired-previous-file . evil-unimpaired-next-file))
;;   (define-minor-mode local-evil-unimpaired-mode
;;     "Local minor mode to provide convient pairs of bindings"
;;     :keymap evil-unimpaired-mode-map
;;     :global nil
;;     (evil-normalize-keymaps))
;;   (add-hook 'prog-mode-hook #'local-evil-unimpaired-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" default)))
 '(dired-sidebar-theme (quote icons))
 '(horizontal-scroll-bar-mode nil)
 '(neo-theme (quote icons))
 '(ns-use-srgb-colorspace t)
 '(package-selected-packages
   (quote
    (evil-unimpaired xterm-color shackle smart-mode-line nyan-mode company-lsp yasnippet helm-ag swiper-helm helm-projectile all-the-icons-dired dired-hacks feature-mode vscode-icon dired-sidebar vscode-icons lsp-typescript lsp-javascript-typescript lsp-haskell lsp-ui evil-easymotion flx prettier-js diminish general avy evil-magit atomic-chrome evil-snipe add-node-modules-path helm hindent mwim yaml-mode docker eyebrowse evil-commentary ivy-hydra hydra evil-anzu anzu tide web-mode ag wgrep counsel-projectile exec-path-from-shell counsel smex ivy hl-todo highlight-todo smartparens tabbar restart-emacs evil-org-agenda evil-org evil-tutor evil-collection evil emojify org-gcal dashboard intero flycheck bash-completion winum all-the-icons magit ztree company undo-tree neotree projectile use-package whole-line-or-region dracula-theme)))
 '(powerline-default-separator (quote bar))
 '(safe-local-variable-values
   (quote
    ((intero-targets "org-tools:lib" "org-tools:test:spec"))))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(spaceline-helm-mode t)
 '(tool-bar-mode nil)
 '(whitespace-style
   (quote
    (face trailing tabs lines empty indentation::tab indentation::space indentation space-after-tab space-before-tab space-mark tab-mark newline-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Source Code Pro"))))
 '(anzu-mode-line ((t (:inherit default-face))))
 '(dashboard-banner-logo-title-face ((t (:inherit default :foreground "#ff79c6" :weight bold))))
 '(dashboard-heading-face ((t (:inherit org-level-2))))
 '(neo-banner-face ((t (:foreground "#ff79c6" :weight bold))))
 '(neo-dir-link-face ((t (:foreground "#8be9fd"))))
 '(neo-expand-btn-face ((t (:foreground "#f8f8f2"))))
 '(neo-file-link-face ((t (:foreground "#f8f8f2"))))
 '(neo-header-face ((t (:foreground "#f8f8f2"))))
 '(neo-root-dir-face ((t (:foreground "#ff79c6" :weight bold))))
 '(powerline-active1 ((t (:background "#a063f6" :foreground "#f8f8f2"))))
 '(powerline-active2 ((t (:background "#a063f6" :foreground "#f8f8f2")))))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
