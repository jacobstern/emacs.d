(package-initialize)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(eval-when-compile
  (require 'use-package))

(defun my-shell-toggle ()
  "Open shell, or quit the window if it's already selected."
  (interactive)
  (if (string-equal (buffer-name) "*shell*")
      (quit-window)
    (shell)))

(defun my-shell-clear-next-output (output)
  "Clear the next output from ComInt and remove this hook."
  (remove-hook 'comint-preoutput-filter-functions 'my-shell-clear-next-output)
  (comint-clear-buffer) output)

(defun my-shell-clear-listener (input)
  (when (string-equal (string-trim input) "clear")
    (add-hook 'comint-preoutput-filter-functions 'my-shell-clear-next-output)))

(add-hook 'shell-mode-hook
	  (lambda () (add-hook 'comint-input-filter-functions
			       'my-shell-clear-listener nil t)))

(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key [f2] 'my-shell-toggle)

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(use-package helm-config)

(use-package helm
  :config
  (helm-mode t)
  :bind (([f1] . helm-M-x)
	 ("M-x" . helm-M-x)
	 ("C-x f" . helm-recentf)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-h a" . helm-apropos)
	 ("M-i" . helm-imenu)))

(defun my-neotree-toggle ()
  "Toggle NeoTree at the project root."
  (interactive)
  (let ((project-dir (projectile-project-root))
	(file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
	(if (neo-global--window-exists-p)
	    (progn
	      (neotree-dir project-dir)
	      (neotree-find file-name)))
      (message "Could not find project root."))))

(use-package neotree
  :after (all-the-icons)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  :bind (([f8] . my-neotree-toggle)))

(use-package projectile
  :ensure t
  :pin melpa-stable
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1))

(use-package whole-line-or-region
  :config
  (whole-line-or-region-global-mode t))

(use-package uniquify
  :init
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-after-kill-buffer-p t)
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package helm-projectile
  :ensure t
  :pin melpa-stable
  :after (projectile)
  :config
  (helm-projectile-on)
  (define-key projectile-mode-map (kbd "M-p") 'helm-projectile-find-file))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(defun my-company-shifted-return ()
  "Abort the current company completion and create a new line."
  (interactive)
  (company-abort)
  ;; This is probably evil but it seems to do a good job executing
  ;; the current binding for return key
  (setq unread-command-events (listify-key-sequence "\r")))

(use-package company
  :config
  (define-key company-active-map [S-return] 'my-company-shifted-return)
  (define-key company-active-map (kbd "<S-return>") 'my-company-shifted-return)
  (add-hook 'comint-mode-hook
	    (lambda ()
	      (setq-local company-active-map
			  (let ((keymap (make-sparse-keymap)))
			    (set-keymap-parent keymap company-active-map)
			    (define-key keymap [tab] 'company-complete-selection)
			    (define-key keymap (kbd "TAB") 'company-complete-selection)
			    (define-key keymap [return] 'my-company-shifted-return)
			    (define-key keymap (kbd "RET") 'my-company-shifted-return) keymap))))
  (add-hook 'shell-mode-hook
	    (lambda ()
	      (define-key shell-mode-map [tab] 'company-complete)
	      (define-key shell-mode-map (kbd "TAB") 'company-complete)))
  (global-company-mode))

(use-package all-the-icons
  :ensure t
  :pin melpa-stable)

(use-package spaceline
  :ensure t
  :pin melpa-stable
  :init
  (setq powerline-default-separator 'wave)
  (setq powerline-image-apple-rgb t)	; Fix colors on macOS
  (setq powerline-height 20)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode 1))

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
;; (use-package spaceline-all-the-icons 
;;   :ensure t
;;   :pin melpa-stable
;;   :after (spaceline all-the-icons)
;;   :init
;;   (setq spaceline-all-the-icons-slim-render t)
;;   :config
;;   (spaceline-all-the-icons-theme))

(use-package org
  :init
  (setq org-log-done t)
  :bind (("C-c l" . org-store-link)
	 ("C-c a" . org-agenda)))

(use-package winum
  :init
  (setq winum-auto-setup-mode-line nil)
  :config
  (winum-mode))

(use-package bash-completion
  :config
  (autoload 'bash-completion-dynamic-complete
    "bash-completion"
    "BASH completion hook")
  (add-hook 'shell-dynamic-complete-functions
	    'bash-completion-dynamic-complete))

;; (use-package dashboard
;;   :ensure t
;;   :after (projectile)
;;   :init
;;   (setq dashboard-startup-banner 'logo)
;;   (setq dashboard-banner-logo-title nil)
;;   (setq dashboard-items '((recents  . 5)
;; 			  (projects . 5)))
;;   :config
;;   (dashboard-setup-startup-hook))
  
;; (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*")))

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
 '(help-window-select t)
 '(horizontal-scroll-bar-mode nil)
 '(menu-bar-mode nil)
 '(neo-theme (quote icons))
 '(ns-alternate-modifier (quote control))
 '(ns-control-modifier (quote meta))
 '(ns-use-srgb-colorspace t)
 '(package-selected-packages
   (quote
    (dashboard intero flycheck bash-completion winum spaceline-all-the-icons all-the-icons spaceline magit ztree company undo-tree neotree helm-projectile projectile use-package whole-line-or-region helm dracula-theme)))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(spaceline-helm-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Source Code Pro"))))
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

