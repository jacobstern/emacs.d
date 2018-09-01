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
  :prefix "C-c SPC")

(defun rockstar-whole-line-or-region-indent (prefix)
  "Indent region or PREFIX whole lines."
  (interactive "*p")
  (whole-line-or-region-call-with-region 'indent-region prefix))

(general-define-key "C-s" 'isearch-forward-regexp)
(general-define-key "C-r" 'isearch-backward-regexp)
(general-define-key "C-M-s" 'isearch-forward)
(general-define-key "C-M-r" 'isearch-backward)
(general-define-key "M-%" 'query-replace-regexp)
(general-define-key "C-M-%" 'query-replace)
(general-define-key "C-<return>" 'newline) ;; Easier to jam the control key

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR."
  t)

(general-define-key "M-z" 'zap-up-to-char)

(setq ring-bell-function 'ignore)
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq enable-recursive-minibuffers t)
(setq confirm-kill-processes nil)

(fset 'yes-or-no-p 'y-or-n-p)
(global-auto-revert-mode t)

(setq-default indent-tabs-mode nil)
(setq-default fill-column 80)

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq ns-control-modifier 'meta)
(setq ns-option-modifier 'control)

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
;; (add-hook 'emacs-lisp-mode-hook (rockstar-add-word-syntax-entry ?-))
;; (add-hook 'emacs-lisp-mode-hook (rockstar-add-word-syntax-entry ?_))
;; (add-hook 'emacs-lisp-mode-hook (rockstar-add-word-syntax-entry ?/))
;; (add-hook 'emacs-lisp-mode-hook (rockstar-add-word-syntax-entry ?*))

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

(use-package mwim
  :ensure t
  :pin melpa
  :config
  (general-define-key "C-a" 'mwim-beginning-of-code-or-line)
  (general-define-key "C-e" 'mwim-end-of-code-or-line))

(use-package counsel
  :ensure t
  :pin melpa
  :after ivy
  :config
  (counsel-mode 1)
  (general-define-key "C-c r" 'counsel-recentf))

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
  (whole-line-or-region-global-mode t)
  (general-define-key "C-M-\\" 'rockstar-whole-line-or-region-indent))

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
  (setq spaceline-window-numbers-unicode t)
  (setq powerline-image-apple-rgb t)  ; Fix colors on macOS
  (setq powerline-height 20)
  (setq powerline-default-separator 'wave)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-toggle-workspace-number-off))

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

;; (use-package evil
;;   :ensure t
;;   :pin melpa
;;   :init
;;   (setq evil-want-C-u-scroll t)
;;   (setq evil-want-integration nil)
;;   (setq evil-normal-state-cursor '(box "#B8779C"))
;;   (setq evil-insert-state-cursor '(box "#87C396"))
;;   (setq evil-emacs-state-cursor '(box "#77ACB8"))
;;   :config
;;   (evil-mode 1)
;;   (evil-set-initial-state 'special-mode 'emacs)
;;   (evil-set-initial-state 'magit-popup-mode 'emacs)
;;   (evil-ex-define-cmd "sh[ell]" 'shell)
;;   (general-emap "C-w" 'evil-window-map)
;;   ;; Note that this forbids us from using Emacs mode to edit text
;;   (general-emap ":" 'evil-ex)
;;   (general-emap "<escape>" 'keyboard-quit))

;; (use-package evil-commentary
;;   :ensure t
;;   :pin melpa
;;   :config
;;   (evil-commentary-mode +1))

(use-package magit
  :ensure t
  :config
  (general-define-key "C-c m" 'magit))

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

;; (use-package evil-org
;;   :ensure t
;;   :pin melpa
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (add-hook 'evil-org-mode-hook
;;             (lambda ()
;;               (evil-org-set-key-theme)))
;;   (general-nmap :keymaps 'evil-org-mode-map "go" 'org-open-at-point)
;;   (general-nmap :keymaps 'evil-org-mode-map "M-o" 'evil-org-org-insert-heading-below)
;;   (general-nmap :keymaps 'evil-org-mode-map "O" nil))

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
  :pin melpa
  :config
  (rockstar-define-leader :keymaps 'ivy-minibuffer-map "SPC" 'hydra-ivy/body)
  (rockstar-define-leader :keymaps 'ivy-minibuffer-map "j" 'ivy-avy)
  (rockstar-define-leader :keymaps 'ivy-minibuffer-map "u" 'ivy-occur))

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
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g '") 'avy-goto-line)
  (avy-setup-default)
  (global-set-key (kbd "C-c C-j") 'avy-resume))

(use-package eyebrowse
  ;; :after evil
  :ensure t
  :pin melpa
  :config
  (eyebrowse-mode t))

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
  (rockstar-define-leader "r" 'counsel-recentf)
  (rockstar-define-leader "j" 'avy-goto-char-timer)
  (rockstar-define-leader "a a" 'org-agenda))

(use-package docker
  :ensure t
  :pin melpa
  :config
  (general-define-key "C-c d" 'docker))

(use-package anzu
  :ensure t
  :pin melpa
  :init
  (setq anzu-cons-mode-line-p nil)
  :config
  (global-anzu-mode +1))

(use-package add-node-modules-path
  :ensure t
  :pin melpa)

;; (use-package evil-anzu
;;   :ensure t
;;   :after (evil anzu)
;;   :pin melpa)

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
  :after (web-mode company-mode)
  :init
  (setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
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
    (mwim yaml-mode docker eyebrowse evil-commentary ivy-hydra hydra evil-anzu anzu tide web-mode add-node-modules-path ag wgrep counsel-projectile exec-path-from-shell counsel smex ivy hl-todo highlight-todo smartparens tabbar restart-emacs evil-org-agenda evil-org evil-tutor evil-collection evil emojify org-gcal dashboard intero flycheck bash-completion winum all-the-icons spaceline magit ztree company undo-tree neotree projectile use-package whole-line-or-region dracula-theme)))
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
 '(spaceline-evil-emacs ((t (:background "#6DDDF5" :foreground "#44475B" :inherit (quote mode-line)))))
 '(spaceline-evil-insert ((t (:background "#4EEE77" :foreground "#44475B" :inherit (quote mode-line)))))
 '(spaceline-evil-normal ((t (:weight normal :background "#ED68B5" :foreground "#44475B" :inherit (quote mode-line)))))
 '(spaceline-evil-replace ((t (:background "#ff79c6" :foreground "#3E3D31" :inherit (quote mode-line)))))
 '(spaceline-evil-visual ((t (:background "#B5B7C6" :foreground "#44475b" :inherit (quote mode-line)))))
 '(spaceline-highlight-face ((t (:background "#70CCE0" :foreground "#44475B" :inherit (quote mode-line))))))

;; Local Variables:
;; eval: (flycheck-mode -1)
;; End:
