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
(smooth-scrolling-mode)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(savehist-mode t)
(set-fill-column 80)
(setq gc-cons-threshold 20000000)
(setq system-uses-terminfo nil)
(set-default 'truncate-lines t)
(hl-line-mode t)
(global-hl-line-mode)
(put 'dired-find-alternate-file 'disabled nil)

;; OSX - Use `Command' key as `alt' key
(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier 'none)

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
(add-hook 'term-mode-hook
          (lambda()
            (whitespace-mode 0)))

;; Remap command to option on Apple Mac
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)

;; Always save the buffer (even if emacs thinks there are no changes)
(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))
(global-set-key (kbd "C-x C-s") 'save-buffer-always)

(defun --prog-mode-hook ()
  (show-paren-mode t)
  (set-fill-column 80)
  (linum-mode t)
  (fci-mode t)
  (hl-line-mode t))
(add-hook 'prog-mode-hook '--prog-mode-hook)

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
;; Configure Packages ----------------------------------------------------------
;; -----------------------------------------------------------------------------

(use-package popwin
  :ensure t
  :config
  (progn
    (setq popwin:special-display-config nil)
    (push '("*Backtrace*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*compilation*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.2)
          popwin:special-display-config)
    (push '("*Compile-Log*"
            :dedicated t :position bottom :stick t :noselect t   :height 0.33)
          popwin:special-display-config)
    (push '("*Help*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*Shell Command Output*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '(" *undo-tree*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*Warnings*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("^\\*Man .*\\*$"
            :regexp t    :position bottom :stick t :noselect nil :height 0.33)
            popwin:special-display-config)
    (popwin-mode t)))

;; TODO - avoid this on OSX?
(use-package xclip
  :ensure t
  :config
  (xclip-mode t))

(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-p") 'ace-window))

(use-package multi-term
  :ensure t
  :config '(setq multi-term-program "/bin/zsh"))

(use-package diminish
  :ensure t
  :init '(diminish 'whitespace-mode))

(use-package dracula-theme
  :ensure t
  :init '(load-theme 'dracula t))

(use-package fill-column-indicator
  :ensure t)

;; Recover the $PATH from the shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

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

  ;; Open projectile-dired on `-`
  (defun maybe-projectile-dired ()
    (interactive)
    (call-interactively
     (if (projectile-project-p)
         #'projectile-dired
       (lambda ()
         (interactive)
         (dired default-directory)))))
  (evil-global-set-key 'normal "-" 'maybe-projectile-dired)
  (evil-global-set-key 'visual "-" 'maybe-projectile-dired)

  ;; Mimic fugitive bindings
  (evil-ex-define-cmd "Gst[atus]" 'magit-status))

;; Tree-based directory browsing
(use-package dired-subtree
  :ensure t
  :init
  (eval-after-load 'dired
    '(progn
       (evil-make-overriding-map dired-mode-map 'normal t)
       (evil-define-key 'normal dired-mode-map
         "o" 'dired-subtree-toggle
       ))))

;; Filtered dired listings
(use-package dired-filter
  :ensure t
  :init
  (eval-after-load 'dired
    '(progn
       (evil-make-overriding-map dired-mode-map 'normal t)
       (evil-define-key 'normal dired-mode-map
         "." 'dired-filter-mode
       ))))

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
  (setq projectile-switch-project-action 'projectile-dired)
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

;; -----------------------------------------------------------------------------
;; Language / Framework support-------------------------------------------------
;; -----------------------------------------------------------------------------

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


;; Yaml
(use-package yaml-mode
  :ensure t
  :config
  (add-hook 'yaml-mode-hook '--prog-mode-hook))

;; Typescript IDE
(use-package typescript-mode
  :ensure t
  :config
  (setq
   typescript-indent-level 2
   typescript-auto-indent-flag 0))

(use-package tide
  :ensure t
  :config
  (add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup))))

;; Dockerfiles
(use-package dockerfile-mode
  :ensure t)

;; Haskell
(use-package haskell-mode
  :ensure t)

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

;; Purescript
(use-package purescript-mode
  :ensure t)

(use-package psc-ide
  :ensure t
  :config
  (add-hook 'purescript-mode-hook
            (lambda ()
              (psc-ide-mode)
              (company-mode)
              (flycheck-mode)
              (turn-on-purescript-indentation)
              (define-key evil-normal-state-map "gd" 'psc-ide-goto-definition))))

;; Apple Swift
(use-package swift-mode
  :ensure t
  :config
  '(setq swift-mode:basic-offset 2))

(use-package flycheck-swift
  :ensure t
  :config
  '(eval-after-load 'flycheck '(flycheck-swift-setup)))
