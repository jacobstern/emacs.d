(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")))

(eval-when-compile (require 'use-package))

(require 'diminish)

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
(general-define-key "M-p" 'query-replace-regexp)
(general-define-key "C-<return>" 'newline) ;; Easier to jam the control key
(general-define-key "C-c o" 'mode-line-other-buffer)

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

(use-package wgrep
  :ensure t
  :pin melpa)

(use-package helm
  :init
  (setq helm-mode-handle-completion-in-region nil)
  ;; :bind (("M-x" . helm-M-x)
  ;;        ("C-x C-f" . helm-find-files)
  ;;        ("C-x C-b" . helm-buffers-list)
  ;;        ("C-h a" . helm-apropos))
  :config
  (helm-mode t)
  (general-define-key :keymaps 'helm-map "<escape>" 'helm-keyboard-quit)
  (general-nmap "C-j" 'helm-imenu)
  (general-define-key "M-x" 'helm-M-x)
  (general-define-key "C-h a" 'helm-apropos))

(use-package helm-projectile
  :ensure t
  :pin melpa
  :after (helm projectile)
  :config
  (helm-projectile-on)
  (general-nmap "C-p" 'helm-projectile))

(use-package helm-ag
  :ensure t
  :pin melpa
  :after helm
  :init
  (setq helm-ag-base-command "rg --no-heading")
  (setq helm-follow-mode-persistent t)
  :config
  (general-nmap "C-s" 'helm-do-ag-project-root))

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
  ;; (add-hook 'neo-enter-hook
  ;;           (lambda (type) (if (equal type 'file)
  ;;                              (neotree-hide))))
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (general-nmap "C-\\" 'rockstar-neotree-toggle)
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
  (projectile-mode 1))

;; (use-package whole-line-or-region
;;   :config
;;   (whole-line-or-region-global-mode t)
;;   (general-define-key "C-M-\\" 'rockstar-whole-line-or-region-indent))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode))

(defun rockstar-company-shifted-return ()
  "Abort the current company completion and create a new line."
  (interactive)
  (company-abort)
  (funcall-interactively (local-key-binding (kbd "<return>"))))


(use-package company
  :diminish company-mode
  :config
  (global-company-mode 1)
  (general-imap "C-SPC" 'company-complete)
  (general-define-key :keymaps 'company-active-map
                      "<S-return>"
                      'rockstar-company-shifted-return)
  (general-define-key :keymaps 'company-active-map "C-n" 'company-select-next)
  (general-define-key :keymaps 'company-active-map
                      "C-p"
                      'company-select-previous))

(use-package all-the-icons
  :ensure t
  :pin melpa)

(use-package all-the-icons-dired
  :ensure t
  :pin melpa
  :after all-the-icons)

(use-package spaceline
  :ensure t
  :pin melpa-stable
  :init
  (setq spaceline-window-numbers-unicode t)
  (setq powerline-image-apple-rgb t)  ; Fix colors on macOS
  (setq powerline-height 20)
  (setq powerline-default-separator nil)
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

;; (use-package intero
;;   :after (haskell-mode flycheck)
;;   :config
;;   (add-hook 'haskell-mode-hook 'intero-mode)
;;   (flycheck-add-next-checker 'intero
;;                              '(warning . haskell-hlint)))

(use-package lsp-mode
  :ensure t
  :pin melpa)

(use-package lsp-ui
  :ensure t
  :pin melpa
  :after lsp-mode
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

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
  (add-hook 'org-agenda-mode-hook #'rockstar-org-gcal-fetch-silently)
  (general-define-key :keymaps 'org-mode-map "<return>" #'org-meta-return))

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
  :config
  (evil-mode 1)
  (evil-ex-define-cmd "sh[ell]" 'shell)
  (general-nmap "M-j" #'evil-window-down)
  (general-nmap "M-k" #'evil-window-up)
  (general-nmap "M-h" #'evil-window-left)
  (general-nmap "M-l" #'evil-window-right))

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
  :config
  (general-define-key "C-c m" 'magit))

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
  (general-nmap :keymaps 'evil-org-mode-map "gx" 'org-open-at-point)
  (general-nmap :keymaps 'evil-org-mode-map "M-o" (evil-org-define-eol-command org-insert-heading))
  (general-nmap :keymaps 'evil-org-mode-map "O" #'evil-org-open-above))

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
  (global-set-key (kbd "C-c C-j") 'avy-resume)
  (general-nmap "C-a" 'avy-goto-char-timer))

(use-package evil-easymotion
  :ensure t
  :pin melpa
  :config
  (define-key evil-motion-state-map (kbd ",") nil)
  ;; Redefine some default motions to enable traversing multiple lines
  ;; https://github.com/PythonNut/evil-easymotion/issues/50
  (evilem-make-motion evilem-motion-forward-word-begin #'evil-forward-word-begin)
  (evilem-make-motion evilem-motion-forward-WORD-begin #'evil-forward-WORD-begin)
  (evilem-make-motion evilem-motion-forward-word-end #'evil-forward-word-end)
  (evilem-make-motion evilem-motion-forward-WORD-end #'evil-forward-WORD-end)
  (evilem-make-motion evilem-motion-backward-word-begin #'evil-backward-word-begin)
  (evilem-make-motion evilem-motion-backward-WORD-begin #'evil-backward-WORD-begin)
  (evilem-make-motion evilem-motion-backward-word-end #'evil-backward-word-end)
  (evilem-make-motion evilem-motion-backward-WORD-end #'evil-backward-WORD-end)
  (evilem-default-keybindings ","))

(use-package eyebrowse
  ;; :after evil
  :ensure t
  :pin melpa
  :init
  (setq eyebrowse-keymap-prefix (kbd "C-c e"))
  :config
  (eyebrowse-mode t))

(use-package prettier-js
  :ensure t
  :pin melpa
  :after (typescript-mode web-mode)
  :config
  (add-hook 'typescript-mode-hook 'prettier-js-mode)
  (add-hook 'web-mode-hook 'prettier-js-mode))

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
  ;; (rockstar-define-leader "x" 'counsel-M-x)
  ;; (rockstar-define-leader "/" 'swiper)
  (rockstar-define-leader ":" 'eval-expression)
  ;; (rockstar-define-leader "t" 'rockstar-neotree-toggle)
  ;; (rockstar-define-leader "s" 'counsel-ag)
  ;; (rockstar-define-leader "b" 'ivy-switch-buffer)
  (rockstar-define-leader "d" 'dired)
  (rockstar-define-leader "n" 'rename-buffer)
  ;; (rockstar-define-leader "m" 'counsel-imenu)
  ;; (rockstar-define-leader "p" 'counsel-projectile)
  ;; (rockstar-define-leader "w" 'counsel-projectile-switch-project) ; "Work on"
  ;; (rockstar-define-leader "f" 'counsel-find-file)
  ;; (rockstar-define-leader "r" 'counsel-recentf)
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
                                       web-mode-code-indent-offset))
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
  ;; Override this function to work with TSX files
  ;; (defun lsp-typescript--language-id (buffer)
  ;;   (let ((ext (file-name-extension (buffer-file-name buffer))))
  ;;     (cond ((equal "ts" ext) "typescript")
  ;;           ((equal "tsx" ext) "typescriptreact")
  ;;           (t "javascript"))))
  
  ;; TODO: Investigate whether this workaround is necessary, also pin version to
  ;; https://github.com/emacs-lsp/lsp-javascript/commit/ab62826962887e82f0bc968817be4fc89a6953e4
  (defun lsp-javascript-typescript--render-string (str)
    (condition-case nil
        (with-temp-buffer
          (delay-mode-hooks (web-mode))
          (insert str)
          (font-lock-ensure)
          (buffer-string))
      (error str)))

  (add-hook 'js-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'js-mode-hook #'rockstar-fix-company-for-javascript-lsp)
  (add-hook 'typescript-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'typescript-mode-hook #'rockstar-fix-company-for-javascript-lsp)
  (add-hook 'web-mode-hook #'lsp-javascript-typescript-enable)
  (add-hook 'web-mode-hook #'rockstar-setup-typescript))

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
 '(dired-sidebar-theme (quote icons))
 '(horizontal-scroll-bar-mode nil)
 '(neo-theme (quote icons))
 '(ns-use-srgb-colorspace t)
 '(package-selected-packages
   (quote
    (helm-ag swiper-helm helm-projectile all-the-icons-dired dired-hacks feature-mode vscode-icon dired-sidebar vscode-icons lsp-typescript lsp-javascript-typescript lsp-haskell lsp-ui evil-easymotion flx prettier-js diminish general avy evil-magit atomic-chrome evil-snipe add-node-modules-path helm hindent mwim yaml-mode docker eyebrowse evil-commentary ivy-hydra hydra evil-anzu anzu tide web-mode ag wgrep counsel-projectile exec-path-from-shell counsel smex ivy hl-todo highlight-todo smartparens tabbar restart-emacs evil-org-agenda evil-org evil-tutor evil-collection evil emojify org-gcal dashboard intero flycheck bash-completion winum all-the-icons spaceline magit ztree company undo-tree neotree projectile use-package whole-line-or-region dracula-theme)))
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
