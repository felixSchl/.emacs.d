(setq load-prefer-newer t)

;; -----------------------------------------------------------------------------
;; Package management ----------------------------------------------------------
;; -----------------------------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("emacs-pe" . "https://emacs-pe.github.io/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; -----------------------------------------------------------------------------
;; Settings --------------------------------------------------------------------
;; -----------------------------------------------------------------------------

(require 'diminish)
(line-number-mode)
(column-number-mode)
(setq inhibit-splash-screen t)
(savehist-mode t)
(set-fill-column 80)
(setq gc-cons-threshold 20000000)
(setq system-uses-terminfo nil)
(set-default 'truncate-lines t)
(hl-line-mode t)
(global-hl-line-mode)

;; Tabs/Spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default tab-stop-list (number-sequence 4 120 4))

(require 'whitespace)
(setq whitespace-line-column 80)
(setq whitespace-style
      '(face
        lines-tail
        tabs
        trailing
        tab-mark empty))
(global-whitespace-mode t)
(diminish 'whitespace-mode)

;; Remap command to option on Apple Mac
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

(defun my-prog-mode-hook ()
  (show-paren-mode t)
  (set-fill-column 80)
  (linum-mode t)
  (fci-mode t)
  (hl-line-mode t))
(add-hook 'prog-mode-hook 'my-prog-mode-hook)


;; Fix fci-mode when used in conjunction with company-mode.
;; https://github.com/company-mode/company-mode/issues/180#issuecomment-55047120
(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)

;; -----------------------------------------------------------------------------
;; Keymappings -----------------------------------------------------------------
;; -----------------------------------------------------------------------------

(global-set-key (kbd "<f2>")
                (lambda
                  ()
                  (interactive)
                  (find-file "~/.emacs.d/init.el")))

;; -----------------------------------------------------------------------------
;; Theme -----------------------------------------------------------------------
;; -----------------------------------------------------------------------------

(use-package atom-one-dark-theme
  :ensure t
  :init
  (load-theme
   (if (display-graphic-p)
       'atom-one-dark
     'tsdh-dark)
   t))

;; -----------------------------------------------------------------------------
;; Configure Packages ----------------------------------------------------------
;; -----------------------------------------------------------------------------

;; Smooth-Scrolling - Retain context during scrolling
(use-package smooth-scrolling
  :ensure t)

;; Flycheck - on-the-fly syntax checking
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook
            (lambda()
              (global-flycheck-mode)
              (diminish 'flycheck-mode))))

;; Key-chord - Key stroke combos
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode t))

;; Evil - VIM emulation layer for emacs
(use-package evil
  :ensure t
  :init
   (setq evil-want-C-i-jump t)
   (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)
  (diminish 'undo-tree-mode)

  ;; "Hybrid" editing style:
  ;; (this allows for keymacs key-bindings in insert-mode)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  ;; Do not use vim mappings in term-mode
  (evil-set-initial-state 'term-mode 'emacs)

  (evil-set-initial-state 'git-rebase-mode 'emacs)

  ;; The fastest way to leave insert mode:
  (dolist (x '("jk" "jK" "JK" "Jk" "kj" "kJ" "KJ" "Kj"))
    (key-chord-define evil-insert-state-map x 'evil-normal-state))

  ;; Quickly comment a line using `evil-commentary`
  (defun comment-and-move (count)
  (interactive "p")
  (let ((current-prefix-arg count))
    (call-interactively 'evil-commentary-line))
  (evil-next-line count))
  (evil-global-set-key 'normal "`" 'comment-and-move)
  (evil-global-set-key 'visual "`" 'comment-and-move)

  ;; Mimic fugitive bindings
  (evil-ex-define-cmd "Gst[atus]" 'magit-status))

;; Evil commentary - Toggle comments
(use-package evil-commentary
  :ensure t
  :config
  (evil-commentary-mode)
  (diminish 'evil-commentary-mode))

;; Magit - A vim porcelain
(use-package magit
  :ensure t
  :init
  (setq magit-diff-paint-whitespace t)
  (setq magit-diff-highlight-trailing t))

;; Company - Auto-completion
(use-package company
  :ensure t
  :init
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-minimum-prefix-length 2)

  :config
  (global-company-mode)
  (diminish 'company-mode))

;; Move `company-files` to the front
(setq company-backends (remove 'company-files company-backends))
(add-to-list 'company-backends 'company-files)

;; Spaceline - A mode line
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-spacemacs-theme))

;; Diff-hl - Highlight changed lines
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  ;; Highlight changes on-the-fly
  (diff-hl-flydiff-mode)
  (unless (display-graphic-p)
    (setq diff-hl-side 'left)
    (diff-hl-margin-mode)))

;; Column-marker - Mark the 80ths column
(use-package column-marker
  :ensure t
  :config
  (column-marker-1 80))

;; Helm - incremental completion framework
(use-package helm
  :ensure t
  :config
  (setq helm-mode-fuzzy-match t)
  (setq helm-completion-in-region-fuzzy-match t)
  (helm-mode t)
  (diminish 'helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "C-c f r") 'helm-recentf)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
  (define-key helm-map (kbd "C-z")  'helm-select-action))

(use-package helm-ls-git
  :ensure t)

;; Projectile - Project interaction library
(use-package projectile
  :ensure t
  :config
  (projectile-global-mode t)
  (diminish 'projectile-mode))

;; Flx-ido - Proper fuzzy matching for ido-mode
(use-package flx-ido
  :ensure t
  :init
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  :config
  (flx-ido-mode t))

;; Helm-flx - Proper fuzzy matching for helm
(use-package helm-flx
  :ensure t
  :config
  (helm-flx-mode t))

;; Ido-vertical - vertical ido completion
(use-package ido-vertical-mode
  :ensure t
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode t))

;; Whitespace butler - clean up whitespace
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode t))

;; Evil-visualstar - Start search with # and * in visual selection
(use-package evil-visualstar
  :ensure t
  :config
  (global-evil-visualstar-mode))

;; Markdown
(use-package markdown-mode
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.text\\'"     . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . gfm-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'"       . gfm-mode))
  :config
  (add-hook 'gfm-mode-hook
            (lambda ()
              (interactive)
              (fci-mode t)
              (set-fill-column 80))))

;; -----------------------------------------------------------------------------
;; Language / Framework support-------------------------------------------------
;; -----------------------------------------------------------------------------

;; Dockerfiles
(use-package dockerfile-mode
  :ensure t)

;; Purescript
(use-package purescript-mode
  :load-path "~/.emacs.d/purescript-mode")

;; Setup Flycheck by PureScript projects
(use-package flycheck-purescript
  :ensure t
  :pin emacs-pe
  :init (add-hook 'purescript-mode-hook #'flycheck-purescript-setup))

;; Anaconda mode - Python mode
(use-package anaconda-mode
  :ensure t
  :config
  (add-hook 'python-mode-hook 'anaconda-mode))

;; Company-anaconda - Company completion backend
(use-package company-anaconda
  :ensure t
  :config
  (add-hook 'python-mode-hook
            (lambda ()
              (interactive)
              (set (make-local-variable 'company-backends)
                   '(company-files
                     (company-dabbrev
                      company-dabbrev-code
                      company-anaconda))))))
