(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
  ("melpa-stable" . "https://stable.melpa.org/packages/")
  ("melpa" . "http://melpa.org/packages/")
  ("marmalade" . "http://marmalade-repo.org/packages/")))

(eval-when-compile (require 'use-package))

(load (expand-file-name "secret" user-emacs-directory))

(require 'general)

(general-evil-setup)

(general-create-definer rockstar-define-leader
  :keymaps 'override
  :prefix "C-c")

(general-mmap "," (general-simulate-key "C-c"))
(general-imap "<f5>" (general-simulate-key "C-c"))
(general-mmap "<f5>" (general-simulate-key "C-c"))

(setq ring-bell-function 'ignore)
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))
(setq confirm-kill-processes nil)
(setq indent-tabs-mode nil)
(setq enable-recursive-minibuffers t)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(menu-bar-mode -1)

(defmacro rockstar-init-mode-indent (variable &rest extras-to-set)
  "Create a function that initializes a mode with a certain
indentation size."
  (let ((base-statements `((setq evil-shift-width ,variable)
                           (setq tab-width ,variable))))
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
  (comint-clear-buffer) output)

(defun rockstar-shell-clear-listener (input)
  (when (string-match-p rockstar-shell-clear-regex (string-trim input))
    (add-hook 'comint-preoutput-filter-functions 'rockstar-shell-clear-next-output)))

(use-package shell
  :init
  (setq comint-prompt-read-only t)
  :config
  (add-hook 'shell-mode-hook
           (lambda () (add-hook 'comint-input-filter-functions
                                'rockstar-shell-clear-listener nil t))))

(when (memq window-system '(mac ns x))
  (use-package exec-path-from-shell
    :ensure t
    :pin melpa
    :config
    (exec-path-from-shell-initialize)))

(use-package smex
  :ensure t
  :pin melpa
  :config
  (smex-initialize))

(use-package ivy
  :ensure t
  :pin melpa
  :after (smex hydra)
  :init
  (setq ivy-use-virtual-buffers t)
  :config
  (ivy-mode 1))

(use-package counsel
  :ensure t
  :pin melpa
  :after ivy
  :config
  (counsel-mode 1))

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

(use-package neotree
  :after (all-the-icons)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

(use-package projectile
  :ensure t
  :pin melpa
  :after ivy
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-mode 1))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode t))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package company
  :init
  (setq company-minimum-prefix-length 3)
  (setq company-abort-manual-when-too-short t)
  :config
  (global-company-mode 1))

(use-package all-the-icons
  :ensure t
  :pin melpa-stable)

(use-package spaceline
  :ensure t
  :pin melpa-stable
  :init
  (setq powerline-default-separator 'wave)
  (setq powerline-image-apple-rgb t)  ; Fix colors on macOS
  (setq powerline-height 20)
  :config
  (spaceline-spacemacs-theme))

(use-package flycheck
  :ensure t
  :pin melpa-stable
  :config
  (add-hook 'after-init-hook 'global-flycheck-mode)
  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))

(use-package intero
  :after (haskell-mode flycheck)
  :config
  (add-hook 'haskell-mode-hook 'intero-mode)
  (flycheck-add-next-checker 'intero
                             '(warning . haskell-hlint)))

(defun rockstar-org-gcal-fetch-silently ()
    (org-gcal-sync nil t t))

(use-package org
  :init
  (setq org-log-done t)
  (setq org-agenda-files (list "~/org/gcal.org" "~/org/planner.org"))
  :config
  (add-hook 'org-agenda-mode-hook 'rockstar-org-gcal-fetch-silently))

(use-package winum
  :init
  (setq winum-auto-setup-mode-line nil)
  :config
  (winum-mode))

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
(setq initial-buffer-choice
      (lambda ()
  (org-agenda-list)
  (let ((agenda-buffer (get-buffer "*Org Agenda*")))
    (let ((agenda-window (get-buffer-window agenda-buffer)))
      (delete-other-windows agenda-window)
      agenda-buffer))))

(use-package emojify
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package evil
  :ensure t
  :pin melpa
  :init
  (setq evil-want-C-u-scroll t)
  (setq evil-want-integration nil)
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "sh[ell]" 'shell))

(use-package evil-commentary
  :ensure t
  :pin melpa
  :config
  (evil-commentary-mode +1))

(use-package evil-collection
  :ensure t
  :pin melpa
  :after (evil ivy ivy-hydra counsel avy)
  :init
  (setq evil-collection-setup-minibuffer t)
  (setq evil-collection-company-use-tng nil)
  :config
  (evil-collection-init)
  ; Prevent normal mode in minibuffer
  (dolist (map (cons 'ivy-minibuffer-map evil-collection-minibuffer-maps))
    (evil-collection-define-key 'insert map
      (kbd "<escape>") 'abort-recursive-edit)))

(use-package evil-org
  :ensure t
  :pin melpa
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme
         '(textobjects insert navigation additional shift todo heading))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package restart-emacs
  :ensure t
  :pin melpa)

(use-package smartparens
  :ensure t
  :pin melpa
  :init
  (setq sp-show-pair-delay 0.2
        sp-show-pair-from-inside t
        sp-cancel-autoskip-on-backward-movement nil
        sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)
  :config
  (require 'smartparens-config)
  (smartparens-global-mode +1)
  (show-smartparens-global-mode +1))

(use-package hydra
  :ensure t
  :pin melpa)

(use-package ivy-hydra
	:after hydra
  :ensure t
  :pin melpa)

(use-package hl-todo
  :ensure t
  :pin melpa
  :config
  (global-hl-todo-mode +1))

(use-package counsel-projectile
  :after counsel
  :ensure t
  :pin melpa
  :config
  (counsel-projectile-mode +1))

(use-package wgrep
  :ensure t
  :pin melpa)

(use-package ag
  :ensure t
  :pin melpa)

(use-package avy
  :ensure t
  :pin melpa
  :config
  (avy-setup-default))

;; Reserved:
;; t - tag
;; r - refac
;; . - auto-fix
;; g - mode specific navigation ?
;; c - commit ?
;; h - special mode / hydra
;; i - info
;; u - occur
;; e - edit ?
(use-package general
  :ensure t
  :pin melpa
  :after (projectile avy)
  :config
  (rockstar-define-leader "TAB" 'mode-line-other-buffer)
  (rockstar-define-leader "x" 'counsel-M-x)
  (rockstar-define-leader "/" 'swiper)
  (rockstar-define-leader ":" 'eval-expression)
  (rockstar-define-leader "t" 'rockstar-neotree-toggle)
  (rockstar-define-leader "s" 'counsel-ag)
  (rockstar-define-leader "b" 'ivy-switch-buffer)
  (rockstar-define-leader "d" 'dired)
  (rockstar-define-leader "n" 'rename-buffer)
  (rockstar-define-leader "m" 'counsel-imenu)
  (rockstar-define-leader "p" 'counsel-projectile)
  (rockstar-define-leader "w" 'counsel-projectile-switch-project) ; "Work on"
  (rockstar-define-leader "f" 'counsel-find-file)
  (rockstar-define-leader "l" 'counsel-recentf) ; "Listing"
  (rockstar-define-leader "a" 'org-agenda)
  (rockstar-define-leader "j" 'avy-goto-char-timer))

(use-package add-node-modules-path
  :ensure t
  :pin melpa)

(use-package anzu
  :ensure t
  :pin melpa
  :init
  (setq anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode +1))

(use-package evil-anzu
  :ensure t
  :after (evil anzu)
  :pin melpa)

(setq rockstar-preferred-web-mode-indent 2)

(use-package web-mode
  :ensure t
  :pin melpa
  :after add-node-modules-path
  :config
  (add-hook 'web-mode-hook
            (rockstar-init-mode-indent rockstar-preferred-web-mode-indent
                                       web-mode-markup-indent-offset
                                       web-mode-code-indent-offset
                                       web-mode-css-indent-offset))
  (add-hook 'javascript-mode-hook
            (rockstar-init-mode-indent rockstar-preferred-web-mode-indent
                                       web-mode-code-indent-offset))
  (add-hook 'js-jsx-mode-hook
            (rockstar-init-mode-indent rockstar-preferred-web-mode-indent
                                       js-indent-level))
  (add-hook 'css-mode-hook
            (rockstar-init-mode-indent rockstar-preferred-web-mode-indent
                                       web-mode-css-indent-offset))
  (add-hook 'web-mode-hook #'add-node-modules-path)
  (add-hook 'javascript-mode-hook #'add-node-modules-path)
  (add-hook 'css-mode-hook #'add-node-modules-path))

(setq rockstar-preferred-ts-indent 2)

(defun rockstar-setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :ensure t
  :pin melpa
  :after web-mode
  :config
  (add-hook 'typescript-mode-hook #'add-node-modules-path)
  (add-hook 'typescript-mode-hook #'rockstar-setup-tide-mode)
  (add-hook 'typescript-mode-hook (rockstar-init-mode-indent rockstar-preferred-ts-indent
                                                             typescript-indent-level))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (rockstar-setup-tide-mode)))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes (quote (dracula)))
 '(custom-safe-themes
   (quote
    ("aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" default)))
 '(horizontal-scroll-bar-mode nil)
 '(neo-theme (quote icons))
 '(ns-use-srgb-colorspace t)
 '(package-selected-packages
   (quote
    (evil-commentary ivy-hydra hydra evil-anzu anzu tide web-mode add-node-modules-path ag wgrep counsel-projectile exec-path-from-shell counsel smex ivy hl-todo highlight-todo smartparens tabbar restart-emacs evil-org-agenda evil-org evil-tutor evil-collection evil emojify org-gcal dashboard intero flycheck bash-completion winum all-the-icons spaceline magit ztree company undo-tree neotree projectile use-package whole-line-or-region dracula-theme)))
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
 '(powerline-active2 ((t (:background "#a063f6" :foreground "#f8f8f2"))))
 '(spaceline-highlight-face ((t (:foreground "#3E3D31" :background "#38d9fc" :inherit (quote mode-line))))))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
