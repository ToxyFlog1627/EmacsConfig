;; Temp theme to avoid blinding light
(load-theme 'wombat)

;; Startup settings
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; Use tabs for indentation
(setq indent-tabs-mode t)

;; Disable shitty bell
(setq ring-bell-function 'ignore)

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/") ("org" . "https://orgmode.org/elpa/") ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Initilize use-package on non-Linux
(unless (package-installed-p 'use-package) 
	(package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; Changing view
(scroll-bar-mode -1) ; Disable scrollbar
(tool-bar-mode   -1) ; Disable toolbar
(tooltip-mode    -1) ; Disable tooltip
(menu-bar-mode   -1) ; Diasble menubar
(set-fringe-mode  8) ; Padding

;; Font
(set-face-attribute 'default nil :font "Menlo" :height 123)

;; Themes
(use-package doom-themes
    :init (load-theme 'doom-one t)) ;; doom-one doom-moonlight doom-snazzy doom-spacegray

;; Counsel
(use-package counsel
    :bind (
        ("M-x" . counsel-M-x)
        ("C-x b" . counsel-ibuffer))
        ("C-x C-f" . counsel-find-file)
    :config (setq ivy-initial-inputs-alist nil))

(global-set-key (kbd "C-M-j") 'counsel-switch-buffer)

;; Ivy
(use-package ivy-rich
    :init (ivy-rich-mode 1))
(use-package ivy
  :diminish
  :bind (("C-s" . swiper) ;; better search
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)	
        ;;  ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
        ;;  ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
        ;;  ("C-d" . ivy-reverse-i-search-kill)
         ("C-k" . ivy-previous-line))
  :config (ivy-mode 1))


;; Packages
(use-package general)
(use-package magit) ;; Git gui
(use-package diminish) ;; Hide minor modes
(use-package all-the-icons) ;; init with "all-the-icons-install-fonts"

;; Org
(defun cstm/org-mode ()
    (org-indent-mode)
    (visual-line-mode 1))

(use-package org
    :hook (org-mode . cstm/org-mode)
    :config 
    (setq org-ellipsis ""
          org-hide-emphasis-markers t
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
        (set-face-attribute (car face) nil :font "Menlo" :weight 'regular :height (cdr face))))

(use-package org-bullets
     :hook (org-mode . org-bullets-mode)
     :custom (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun cstm/org-mode-visual-fill ()
    (setq visual-fill-column-width 100
          visual-fill-column-center-text t)
    (visual-fill-column-mode 1))

(use-package visual-fill-column
    :hook (org-mode . cstm/org-mode-visual-fill))

(org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (shell . t)
      (javascript . t)))

(use-package org-tempo
    :init 
    (dolist (template '(
        ("sh" . "src shell")
        ("el" . "src emacs-lisp")
        ("js" . "src javascript")))
        (add-to-list 'org-structure-template-alist template)))

;; Mode line / Status bar
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 5)))

;; Better help commands
(use-package helpful
    :custom
    (counsel-describe-function-function #'helpful-callable)
    (counsel-describe-variable-function #'helpful-variable)
    :bind
    ([remap describe-function] . counsel-describe-function)
    ([remap describe-command] . helpful-command)
    ([remap describe-variable] . counsel-describe-variable)
    ([remap describe-key] . helpful-key))

;; ESC to cancel operation
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Line numbers
(global-display-line-numbers-mode t)
(dolist 
    (mode '(org-mode-hook term-mode-hook eshell-mode-hook shell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow parantheses
(use-package rainbow-delimiters 
    :hook (prog-mode . rainbow-delimiters-mode))

;; Keybind suggestions
(use-package which-key
    :init (which-key-mode)
    :diminish which-key-mode
    :config (setq which-key-idle-delay 0.3))

;; Evil
(use-package evil
    :init
    (setq evil-want-integration t)
    (setq evil-want-C-u-scroll t)
    (setq evil-want-keybinding nil)
    (setq evil-want-C-i-jump nil)
    (setq evil-split-window-below t)
    (setq evil-vsplit-window-right t)
    :config
    (evil-mode 1)

    (define-key evil-insert-state-map (kbd "j j") 'evil-normal-state)
    (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state) 

    (evil-global-set-key 'motion "j" 'evil-next-visual-line)
    (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
    
    (evil-set-initial-state 'messages-buffer-mode 'normal)
    (evil-set-initial-state 'dashboard-mode 'normal))

(use-package evil-collection
    :after evil
    :config (evil-collection-init))

;; Projectile
(use-package projectile
    :diminish projectile-mode
    :config (projectile-mode)
    :custom ((projectile-completion-system 'ivy))
    :bind-keymap 
    ("C-c p" . projectile-command-map)
    :init
    (when (file-directory-p "~/Documents/") (setq projectile-project-search-path '("~/Documents/")))
    (setq projectile-switch-project-action #'projectile-dired))

(use-package counsel-projectile
    :config (counsel-projectile-mode))












;; !Autoadded
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(which-key rainbow-delimiters doom-modeline counsel diminish use-package))
 '(warning-suppress-log-types '((comp) (comp)))
 '(warning-suppress-types '((comp))))
