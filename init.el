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

(global-set-key (kbd "M-o") 'mode-line-other-buffer)
(global-set-key [f1] 'my-shell-toggle)

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(defun my-shell-clear-next-output (output)
  (remove-hook 'comint-preoutput-filter-functions 'my-shell-clear-next-output)
  (comint-clear-buffer) output)

(defun my-shell-clear-listener (input)
  (when (string-equal (string-trim input) "clear")
    (add-hook 'comint-preoutput-filter-functions 'my-shell-clear-next-output)))

(add-hook 'shell-mode-hook
	  (lambda () (add-hook 'comint-input-filter-functions
			       'my-shell-clear-listener nil t)))

(use-package helm-config)

(use-package helm
  :config
  (helm-mode t)
  :bind (([f2] . helm-M-x)
	 ("C-x M-f" . helm-recentf)
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
  (whole-line-or-region-mode t))

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
  (helm-projectile-on))

(use-package undo-tree
  :config
  (global-undo-tree-mode))

(use-package company
  :config
  (global-company-mode))

(use-package all-the-icons
  :ensure t
  :pin melpa-stable)

(use-package spaceline
  :ensure t
  :pin melpa-stable
  :init
  (setq powerline-default-separator 'slant)
  (setq powerline-height 20)
  :config
  (spaceline-spacemacs-theme)
  (spaceline-helm-mode 1))

;; (use-package spaceline-all-the-icons 
;;   :ensure t
;;   :pin melpa-stable
;;   :after (spaceline all-the-icons)
;;   :init
;;   (setq spaceline-all-the-icons-slim-render t)
;;   :config
;;   (spaceline-all-the-icons-theme))
 
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

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
 '(package-selected-packages
   (quote
    (spaceline-all-the-icons all-the-icons spaceline magit ztree company undo-tree neotree helm-projectile projectile use-package whole-line-or-region helm dracula-theme)))
 '(save-place-mode t)
 '(scroll-bar-mode nil)
 '(sentence-end-double-space nil)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282a36" :foreground "#f8f8f2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Source Code Pro")))))
