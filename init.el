(setq cstm/project-dirs '("~/Documents/Web" "~/Documents/Other"))

;; Increase memory size to 75MB to decrease startup time
(setq gc-cons-threshold (* 75 1024 1024))

(load-theme 'wombat)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Don't create temp lock files
(setq create-lockfiles nil)

;; Use tabs instead of spaces
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default default-tab-width 4)

;; Disable shitty bell
(setq ring-bell-function 'ignore)

(when (eq system-type 'darwin) (customize-set-variable 'native-comp-driver-options '("-Wl,-w")))
(setq byte-compile-warnings '(cl-functions))

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/") 
                         ("org" . "https://orgmode.org/elpa/") 
                         ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Initilize use-package on non-Linux
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package auto-package-update
    :custom
    (auto-package-update-interval 7)
    (auto-package-update-prompt-before-update nil)
    (auto-package-update-hide-results t)
    :config
    (auto-package-update-maybe)
    (auto-package-update-at-time "19:30"))

(use-package diminish)

(use-package no-littering
  :custom (setq auto-save-file-name-transforms 
                `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))

(setq backup-directory-alist '(("." . "~/.emacs.d/emacs-backups")))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-undo-system 'undo-redo)
  :config
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) 
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-nerd-commenter :bind ("M-/" . evilnc-comment-or-uncomment-lines))

(use-package general)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; ESC = C-g

(custom-set-variables '(initial-frame-alist (quote ((fullscreen . maximized)))))

(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

(scroll-bar-mode -1) ; Disable scrollbar
(tool-bar-mode   -1) ; Disable toolbar
(tooltip-mode    -1) ; Disable tooltip
(menu-bar-mode   -1) ; Diasble menubar
(set-fringe-mode  8) ; Padding

(set-face-attribute 'default nil :font "Menlo" :height 123)

(use-package doom-themes :init (load-theme 'doom-one t))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 5)))

(use-package all-the-icons)

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file))
  :config 
  (setq ivy-initial-inputs-alist nil)
  (global-set-key (kbd "C-M-j") 'counsel-switch-buffer))

(use-package ivy-rich :init (ivy-rich-mode 1))
(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line))
  :config (ivy-mode 1))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer))

(use-package dired-single 
  :commands (dired dired-jump))

(use-package all-the-icons-dired 
  :commands (dired dired-jump)
  :hook (dired-mode . all-the-icons-dired-mode))

(defun cstm/org-mode ()
    (org-indent-mode)
    (visual-line-mode 1))

(use-package org
    :pin org
    :commands (org-capture)
    :hook (org-mode . cstm/org-mode)
    :config 
    (setq org-hide-emphasis-markers t
          org-confirm-babel-evaluate nil)
    (dolist (face '(
        (org-level-1 . 1.2)
        (org-level-2 . 1.15)
        (org-level-3 . 1.1)
        (org-level-4 . 1.05)
        (org-level-5 . 1.025)
        (org-level-6 . 1.0)
        (org-level-7 . 1.0)
        (org-level-8 . 1.0)))
        (set-face-attribute (car face) nil :font "Menlo" :weight 'regular :height (cdr face)))
    (dolist (template '(
        ("sh" . "src shell")
        ("el" . "src emacs-lisp")))
        (add-to-list 'org-structure-template-alist template)))

(use-package org-bullets
     :hook (org-mode . org-bullets-mode)
     :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun cstm/org-mode-visual-fill ()
    (setq visual-fill-column-width 120
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
    :hook (org-mode . cstm/org-mode-visual-fill))

(with-eval-after-load 'org
    (org-babel-do-load-languages
     'org-babel-load-languages
        '((emacs-lisp . t)
        (shell . t))))

(defun cstm/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name)) (expand-file-name user-emacs-directory))
    (let ((org-confirm-babel-evaluate nil)) (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'cstm/org-babel-tangle-config)))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :custom
  (lsp-enable-file-watchers nil)
  (lsp-enable-links nil)
  (lsp-enable-which-key-integration t)
  (lsp-origami-mode t)
  (lsp-headerline-breadcrumb-enable nil))

(use-package lsp-origami :after lsp)

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy :after lsp)

(use-package origami
  :bind (:map origami-mode-map
              ("C-c @ C-c" . origami-toggle-node)
              ("C-c @ C-l" . origami-recursively-toggle-node)))

(use-package company
  :after lsp-mode
  :hook (lsp-mode . company-mode)
  :bind 
  (:map company-active-map ("<tab>" . company-complete-selection))
  (:map lsp-mode-map ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0))

(use-package company-box :hook (company-mode . company-box-mode))

(use-package magit :commands magit-status)

(use-package web-mode :mode "\\.[tj]sx?$")

(use-package emmet-mode 
  :config 
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook #'(lambda () (setq-local emmet-expand-jsx-className? t))))

(use-package rjsx-mode :config (add-hook 'web-mode-hook 'rjsx-mode))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'"
  :interpreter "json"
  :custom (js-indent-level 2))

(use-package typescript-mode
  :mode "\\.tsx?\\'"
  :hook (typescript-mode . lsp-deferred))

(use-package exec-path-from-shell :config (exec-path-from-shell-initialize))
(use-package flycheck :config (global-flycheck-mode))

(setq-default flycheck-disabled-checkers (append flycheck-disabled-checkers '(javascript-jshint json-jsonlist)))

(flycheck-add-mode 'javascript-eslint 'web-mode)

(use-package add-node-modules-path :config (add-hook 'flycheck-mode-hook 'add-node-modules-path))

(use-package prettier-js
  :custom
  (prettier-js-args '(
                      "--bracket-same-line" "false"
                      "--allow-parens" "avoid"
                      "--bracket-spacing" "false"
                      "--use-tabs" "true"
                      "--semi" "true"
                      "--single-quote" "false"
                      "--jsx-single-quote" "false"
                      "--trailing-comma" "es5"
                      "--tab-width" "1"
                      "--print-width" "180"
                      ))
  :config
  (add-hook 'web-mode-hook #'(lambda ()
                               (enable-minor-mode '("\\.jsx?\\'" . prettier-js-mode)))))

(use-package vterm
    :commands vterm
    :config 
    (setq vterm-shell "zsh")
    (setq vterm-max-scrollback 5000))

(use-package rainbow-delimiters :hook (prog-mode . rainbow-delimiters-mode))

(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook term-mode-hook eshell-mode-hook shell-mode-hook git-commit-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(use-package projectile
  :diminish projectile-mode
  :config 
  (projectile-mode)
  (setq projectile-enable-caching t)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/") 
    (setq paths '())
    (dolist (dir cstm/project-dirs)
      (setq paths (append paths (cddr (remove-if (lambda (el) (or (not (file-directory-p el)) (member el '("." ".." ".DS_STORE")))) (directory-files dir))))))
    (setq projectile-project-search-path paths)
  (setq projectile-switch-project-action #'projectile-dired)))

(use-package counsel-projectile 
  :after projectile
  :config (counsel-projectile-mode))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config (setq which-key-idle-delay 0.3))

;; Decrease memory to 2MB
(setq gc-cons-threshold (* 2 1024 1024))
