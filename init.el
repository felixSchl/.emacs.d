;;; init.el --- Initialization file for Emacs
;;; Commentary:
;; Emacs Startup File --- initialization for Emacs
;;; Code:

(require 'package)
(setq
 package-enable-at-startup nil
 package-archives '(("elpa" . "http://elpa.gnu.org/packages/")
;                    ("emacs-pe" . "https://emacs-pe.github.io/packages/")
;                   ("marmalade" . "https://marmalade-repo.org/packages/")
                    ("melpa" . "http://melpa.org/packages/")
                    ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/vendor")

(setq exec-path
      '("~/.local/bin"
        "~/bin"
        "~/go/bin/"
        "~/.ghcup/bin"
        "~/.nvm/versions/node/v14.13.1/bin"
        "/bin"
        "/usr/bin"
        "/usr/local/go/bin"
        "/usr/local/bin"))
(setenv "PATH" (string-join exec-path ":"))

(eval-when-compile
  (require 'use-package))

;; Uncomment this to turn on use-package diagnostics
;; Use `use-package-report' to view a report after loading file.
;; (setq use-package-compute-statistics t)

;; -----------------------------------------------------------------------------
;; Utility functions
;; -----------------------------------------------------------------------------

(defun my/require-softly (feature &optional filename)
  "As `require', but instead of an error just print a message.
If there is an error, its message will be included in the message
printed.
Like `require', the print name of FEATURE is used FILENAME is omitted.
Like `require', the return value will be FEATURE if the load was
successful (or unnecessary) and nil if not."
  (condition-case err
      (require feature filename)
    (error (message "Error loading %s: \"%s\""
                    (if filename (format "%s (%s)" feature filename) feature)
                    (error-message-string err))
           nil)))

;; -----------------------------------------------------------------------------
;; Stock Settings
;; -----------------------------------------------------------------------------

(setq inhibit-splash-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(savehist-mode t)
(setq-default truncate-lines t)
(global-hl-line-mode t)
(setq-default fill-column 80)
(with-eval-after-load 'display-fill-column-indicator ;; Emacs 27+ only
    (global-display-fill-column-indicator-mode))
(my/require-softly 'display-fill-column-indicator)
(with-eval-after-load 'ido
  (ido-everywhere t))

;; OSX - Use `Command' key as `alt' key
(setq
  mac-option-key-is-meta nil
  mac-command-key-is-meta t
  mac-command-modifier 'meta
  mac-option-modifier 'none)

;; Configure display of whitespace
(require 'whitespace)
(setq whitespace-line-column 80
      whitespace-style '(face
                         lines-tail
                         tabs
                         trailing
                         tab-mark empty))
(global-whitespace-mode t)

;; Configure term-mode
(defun my/stock-term-hook ()
  "Custom Term Hook."
  (whitespace-mode 0))
(with-eval-after-load 'term-mode
  (add-hook 'term-mode-hook 'my/stock-term-hook))

;; Always save the buffer (even if emacs thinks there are no changes)
(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))
(global-set-key (kbd "C-x C-s") 'save-buffer-always)

;; Toggle-Maximize a buffer
;; TODO could be a bit cleverer. It's easy to mess up using this and lose
;; windows forever.
(defun my/toggle-maximize-buffer ()
  "Maximize buffer."
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
(global-set-key [(meta shift return)] 'my/toggle-maximize-buffer)

;; -----------------------------------------------------------------------------
;; Built-in file explorer (Dired)
;; -----------------------------------------------------------------------------
(defun my/dired-hook ()
  "Custom Dired Hook."
  ;; Auto-revert dired buffers when files change on disk
  (auto-revert-mode t)
  (dired-omit-mode t)
  (setq dired-omit-verbose 0)

  ;; More evil support
  (evil-make-overriding-map dired-mode-map 'normal t)
  (evil-define-key 'normal dired-mode-map "g" 'revert-buffer)
  (my/require-softly 'dired-subtree)
  (if (featurep 'dired-subtree)
      (evil-define-key 'normal dired-mode-map "o" 'dired-subtree-toggle)))
(add-hook 'dired-mode 'my/dired-hook)

;; dired-x provides `C-x C-j' to quickly open current buffer's directory
(use-package dired-x :ensure nil)

;; pass integration
;; see: https://blog.drshapeless.com/post/7
(use-package pass :ensure t
  :config
  (use-package pass)
  (require 'epa-file)
  (setq epg-pinentry-mode 'loopback)
  (setq auth-sources '(password-store)))

(use-package helm-pass
  :ensure t
  :after helm
  :config
  (global-set-key (kbd "C-x p") 'helm-pass))

;; -----------------------------------------------------------------------------
;; Shell Code
;; -----------------------------------------------------------------------------
(defun my/sh-mode-hook ()
  "Custom `sh-mode' Hook."
  (setq-local
   indent-tabs-mode t
   tab-width 4))
(add-hook 'sh-mode-hook 'my/sh-mode-hook)

;; -----------------------------------------------------------------------------
;; SQL
;; -----------------------------------------------------------------------------

;; PostgreSQL
(defun pgsql-scratch ()
  "Open PostgreSQL scratch space."
  (interactive)
  (switch-to-buffer "*pgsql-scratch*")
  (sql-mode)
  (sql-set-product "postgres")
  (sql-set-sqli-buffer))

;; MySQL
(defun mysql-scratch ()
  "Open MySQL scratch space."
  (interactive)
  (switch-to-buffer "*mysql-scratch*")
  (sql-mode)
  (sql-set-product "mysql")
  (sql-set-sqli-buffer))

;; -----------------------------------------------------------------------------
;; C Code
;; -----------------------------------------------------------------------------
(defun my/c-mode-hook ()
  "Custom `c-mode' hook."
  (setq
   backward-delete-char-untabify-method nil
   c-basic-offset 8
   tab-width 8
   indent-tabs-mode t)
  (add-hook 'before-save-hook 'clang-format-buffer nil t)
  (with-eval-after-load 'evil
    (evil-define-key 'normal c-mode-map
      "gd" 'xref-find-definitions)))
(add-hook 'c-mode-hook 'my/c-mode-hook)

;; -----------------------------------------------------------------------------
;; Prog mode
;; -----------------------------------------------------------------------------
(defun my/prog-mode-hook ()
  "Base configuration for 'prog-mode'."
  (setq indent-tabs-mode nil)
  (show-paren-mode t)
  (set-fill-column 80)
  (abbrev-mode t)
  (hl-line-mode t)
  (flyspell-prog-mode))
(add-hook 'prog-mode-hook 'my/prog-mode-hook)

;; -----------------------------------------------------------------------------
;; packages
;; -----------------------------------------------------------------------------

(use-package undo-fu
  :ensure t)

(use-package clang-format
  :ensure t)

;; Fill column indicator to visually see the fill column
;; Note: Only enabled for Emacs < 27. Emacs 27+ sports built-in support for this
;;       via `display-fill-column-indicator'.
(use-package fill-column-indicator
  :unless (featurep 'display-fill-column-indicator)
  :ensure t
  :config
  ;; Fix fci-mode when used in conjunction with company-mode.
  ;; https://github.com/company-mode/company-mode/issues/180#issuecomment-55047120
  (with-eval-after-load 'company
    (defvar-local company-fci-mode-on-p nil)
    (defun company-turn-off-fci (&rest ignore)
      (when (boundp 'fci-mode)
        (setq company-fci-mode-on-p fci-mode)
        (when fci-mode (fci-mode -1))))
    (defun company-maybe-turn-on-fci (&rest ignore)
      (when company-fci-mode-on-p (fci-mode 1)))
    (add-hook 'company-completion-started-hook 'company-turn-off-fci)
    (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
    (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)))

;; Tree-based directory browsing
(use-package dired-subtree
  :ensure t
  :defer t)

;; Evil - Vim emulation layer for emacs
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
        evil-respect-visual-line-mode t
        evil-want-C-i-jump t
        evil-want-C-u-scroll t
        evil-undo-system 'undo-fu)
  :config
  (defun my/save-buffer ()
    "Save the buffer but don't muck up evil-repeat."
    (interactive)
    (set-buffer-modified-p t)
    (save-buffer nil))
  (evil-mode t)

  ;; Drop into normal mode after saving file
  (add-hook 'after-save-hook #'evil-normal-state)

  ;; Do not consider saving a buffer a repeatable action
  (evil-declare-not-repeat 'my/save-buffer)

  ;; "Hybrid" editing style:
  ;; (this allows for keymacs key-bindings in insert-mode)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  ;; flip c-v and v visual modes
  (define-key evil-normal-state-map (kbd  "v") 'evil-visual-block)
  (define-key evil-normal-state-map (kbd "C-v") 'evil-visual-char)
  (define-key evil-visual-state-map (kbd "v") 'evil-visual-block)
  (define-key evil-visual-state-map (kbd "C-v") 'evil-visual-char)

  ;; The fastest way to leave insert mode
  (with-eval-after-load 'key-chord
    (dolist (x '("jk" "jK" "JK" "Jk" "kj" "kJ" "KJ" "Kj"))
        (key-chord-define evil-insert-state-map x 'evil-normal-state)))

  ;; Configure term-mode integration
  (with-eval-after-load 'term-mode
    (evil-set-initial-state 'term-mode 'emacs))

  ;; Configure magit integration
  (with-eval-after-load 'magit
    (evil-set-initial-state 'git-rebase-mode 'emacs)
    (add-hook 'git-commit-mode-hook 'evil-insert-state)))

;; Evil surround mode
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode t))

(defun my/magit-mode-hook ()
  "Custom magit-mode hook."
  (when (require 'helm nil 'noerror)
    (helm-mode t)))

;; Emacs git frontned
(use-package magit
  :ensure t
  :hook (magit-status . magit-mode)
  :init
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq magit-diff-paint-whitespace t
        magit-diff-highlight-trailing t)
  :config
  (add-hook 'magit-mode-hook 'my/magit-mode-hook))

(defun my/add-node-modules-path ()
  "Add node_modules/.bin to 'exec-path' when working with node projects."
  (let ((root (projectile-project-root)))
    (when root
         (let ((bindir (expand-file-name "node_modules/.bin/" root)))
           (when (file-directory-p bindir)
             (make-local-variable 'exec-path)
             (add-to-list 'exec-path bindir))))))

;; Projectile - Project interaction library
(use-package projectile
  :ensure t
  :diminish projectile-mode
  :delight '(:eval (concat " " (projectile-project-name)))
  :config
  (projectile-mode t)
  (eval-after-load 'typescript
    (add-hook 'typescript-mode-hook 'my/add-node-modules-path))
  (setq projectile-switch-project-action 'projectile-dired
        projectile-use-git-grep t)
  ;; (with-eval-after-load 'helm
  ;;   (global-set-key (kbd "C-c g f") 'helm-projectile-grep))
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; A set of useful evil bindings
(use-package evil-collection
  :ensure t
  :after evil
  :custom
  (evil-collection-outline-bind-tab-p t "Bind <tab> in outline mode")
  :config
  (with-eval-after-load 'pass (evil-collection-pass-setup))
  (with-eval-after-load 'calendar (evil-collection-calendar-setup))
  (with-eval-after-load 'dired (evil-collection-dired-setup))
  (with-eval-after-load 'term (evil-collection-term-setup))
  (with-eval-after-load 'term (evil-collection-term-setup))
  (with-eval-after-load 'outline (evil-collection-outline-setup))
  (with-eval-after-load 'magit (evil-collection-magit-setup))
  (with-eval-after-load 'mu4e (evil-collection-mu4e-setup))
  (with-eval-after-load 'flycheck (evil-collection-flycheck-setup))
  (with-eval-after-load 'xref (evil-collection-xref-setup))
  (with-eval-after-load 'tide (evil-collection-tide-setup)))

;; Evil commentary - Toggle comments
(use-package evil-commentary
  :ensure t
  :after evil
  :diminish evil-commentary-mode
  :config
  (evil-commentary-mode))

;; Evil-visualstar - Start search with # and * in visual selection
(use-package evil-visualstar
  :ensure t
  :after evil
  :config
  (global-evil-visualstar-mode))

;; Diff-hl - Highlight changed lines
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode))

;; Use nlinum mode over normal linum mode
(use-package nlinum
  :ensure t
  :config
  :hook (prog-mode . nlinum-mode))

;; Spaceline - A mode line
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  :config
  (spaceline-define-segment buffer-id
    (if (and (featurep 'projectile) buffer-file-name)
        (let ((project-root (projectile-project-p)))
          (if project-root
              (file-relative-name (buffer-file-name) project-root)
              (abbreviate-file-name (buffer-file-name))))
      (powerline-buffer-id)))
  (spaceline-spacemacs-theme)
  (spaceline-toggle-minor-modes-off))

;; Integrate with `keychain` utility
(use-package keychain-environment
  :ensure t)

;; Grab www links to sources at point
(use-package git-link
  :ensure t
  :defer t
  :init
  (setq git-link-use-commit t))

;; Use the system clipboard
;; TODO - avoid this on OSX?
(use-package xclip
  :ensure t
  :config
  (xclip-mode t))

;; Get the temporary buffers under control
(use-package popwin
  :ensure t
  :config
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
    (push '("*undo-tree*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("*Warnings*"
            :dedicated t :position bottom :stick t :noselect nil :height 0.33)
          popwin:special-display-config)
    (push '("^\\*Man .*\\*$"
            :regexp t    :position bottom :stick t :noselect nil :height 0.33)
            popwin:special-display-config)
    (popwin-mode t))

;; Efficient window management and navigation
(use-package ace-window
  :ensure t
  :defer t
  :init
  (global-set-key (kbd "M-o") 'ace-window))

;; Key-chord - Key stroke combos
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode t))

;; Monokai theme
(use-package monokai-theme
  :ensure t
  :config (load-theme 'monokai t))

(use-package irony
  :ensure t
  :config
  (add-hook 'c-mode-common-hook 'irony-mode))

;; Company - Auto-completion
(use-package company
  :ensure t
  :diminish company-mode
  :custom
  (company-dabbrev-downcase nil)
  :init
  (setq company-selection-wrap-around t
        company-show-numbers t
        company-minimum-prefix-length 0)
  :config
  (global-company-mode)

  ;; Move `company-files` to the front
  (setq company-backends (remove 'company-files company-backends))
  (add-to-list 'company-backends 'company-files))

(defun my/flycheck-error-list-mode-hook ()
  "Custom *Flycheck Errors* buffer hook."
  (visual-line-mode t))

;; Flycheck - on-the-fly syntax checking
(use-package flycheck
  :ensure t
  :config
  (add-hook 'flycheck-error-list-mode 'my/flycheck-error-list-mode-hook)
  (global-flycheck-mode t))

;; Terminal emulators inside emacs
;; TODO configure this properly
(use-package multi-term
  :ensure t
  :hook multi-term-mode
  :config '(setq multi-term-program "/bin/zsh"))

;; Helm - incremental completion framework
(use-package helm
  :ensure t
  :defer nil
  :after (projectile)
  :custom
  ((helm-mode-fuzzy-match t)
   (helm-completion-in-region-fuzzy-match t))
  :bind
  (("M-x"     . helm-M-x)
   ("M-y"     . helm-show-kill-ring)
   ("C-x b"   . helm-mini)
   ("C-c f r" . helm-recentf)
   ("C-x C-f" . helm-find-files))
  :init
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'helm)
    (with-eval-after-load 'helm-projectile
      (progn
        (helm-projectile-on)
        (setq helm-projectile-set-input-automatically nil)))))

(use-package helm-mode
  :ensure nil ;; via `helm' package
  :diminish helm-mode
  :config
  (bind-keys :map helm-map
             ("<tab>" . helm-execute-persistent-action)
             ("C-i"   . helm-execute-persistent-action)
             ("C-z"   . helm-select-action)))

;; Eyebrowse - Multiple window configurations
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode 1)
  (let ((m eyebrowse-mode-map))
    (define-key m (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
    (define-key m (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    (define-key m (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    (define-key m (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    (define-key m (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
    (define-key m (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
    (define-key m (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
    (define-key m (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
    (define-key m (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
    (define-key m (kbd "M-9") 'eyebrowse-switch-to-window-config-9)))

;; Whitespace butler - clean up whitespace
(use-package ws-butler
  :ensure t
  :config
  (ws-butler-global-mode t))

;; ;; Dired .gitignore integration
;; (use-package dired-gitignore
;;   :pin manual
;;   :load-path "vendor"
;;   :config
;;   (with-eval-after-load 'evil
;;     (evil-define-key 'normal dired-mode-map
;;       (kbd "h") 'dired-gitignore-mode)))

;; Interactive git grep
(use-package helm-git-grep
  :pin manual
  :load-path "vendor"
  :config
  (with-eval-after-load 'helm
    (global-set-key (kbd "C-c g f") 'helm-git-grep)))

;; ;; Semgrep module
;; (use-package semgrep
;;   :pin manual
;;   :load-path "vendor")


;; Ido-vertical - vertical ido completion
(use-package ido-vertical-mode
  :pin manual
  :load-path "vendor"
  :init
  (setq ido-vertical-define-keys 'C-n-and-C-p-only)
  :config
  (ido-vertical-mode t))

;; Flx-ido - Proper fuzzy matching for ido-mode
(use-package flx-ido
  :ensure t
  :init
  (setq
   ido-enable-flex-matching t
   ido-use-faces nil)
  :config
  (flx-ido-mode t))

;; Helm-flx - Proper fuzzy matching for helm
(use-package helm-flx
  :ensure t
  :config
  (helm-flx-mode t))

;; Smooth-Scrolling - Retain context during scrolling
(use-package smooth-scrolling
  :ensure t
  :init
  (setq scroll-margin 1
        scroll-conservatively 0
        scroll-up-aggressively 0.01
        scroll-down-aggressively 0.01))

(use-package web-mode
  :ensure t
  :mode "\\.html\\'"
  :init
  (setq web-mode-enable-engine-detection t
        web-mode-markup-indent-offset 2)
  :config
  (add-hook 'hack-local-variables-hook
            (defun web-mode-fix-dirlocals+ (&rest _)
              (when (derived-mode-p major-mode 'web-mode)
                (web-mode-guess-engine-and-content-type)))))

; ------------------------------------------------------------------------------
; Language Support
; ------------------------------------------------------------------------------

(use-package kotlin-mode
  :mode "\\.kt\\'"
  :ensure t)

(use-package cmake-mode
  :mode "\\CMakeLists.txt\\'"
  :ensure t)

(defun my/lsp-mode-hook ()
  "Custom LSP Mode Hook.")

(use-package lsp-mode
  :ensure t
  :commands (lsp-mode lsp-deferred)
  :init
  (add-hook 'lsp-mode-hook 'my/lsp-mode-hook))

(use-package lsp-ui
  :after lsp-mode
  :ensure t)

(use-package lsp-java
  :after lsp-mode
  :ensure t)

;; Apple Swift language support
(use-package swift-mode
  :ensure t
  :mode "\\.swift\\'"
  :config
  (my/require-softly 'flycheck-swift)
  (setq swift-mode:basic-offset 2))

;; Apple Swift flycheck integration
(use-package flycheck-swift
  :ensure t
  :defer t
  :after flycheck
  :config
  (flycheck-swift-setup))

;; Typescript language support
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'" "\\.js\\'" "\\.jsx'")
  :init
  (setq
   typescript-indent-level 2
   typescript-auto-indent-flag 0))

(use-package prettier-js
  :ensure t)

;; (progn
;;   (flycheck-remove-next-checker 'typescript-tide 'javascript-eslint)
;;   (flycheck-remove-next-checker 'tsx-tide 'javascript-eslint))

;;(require 'nodejs-repl)
;;(add-hook 'js-mode-hook
;;          (lambda ()
;;            (define-key js-mode-map (kbd "C-x C-e") 'nodejs-repl-send-last-expression)
;;            (define-key js-mode-map (kbd "C-c C-j") 'nodejs-repl-send-line)
;;            (define-key js-mode-map (kbd "C-c C-r") 'nodejs-repl-send-region)
;;            (define-key js-mode-map (kbd "C-c C-c") 'nodejs-repl-send-buffer)
;;            (define-key js-mode-map (kbd "C-c C-l") 'nodejs-repl-load-file)
;;            (define-key js-mode-map (kbd "C-c C-z") 'nodejs-repl-switch-to-repl)))

;; Typescript IDE
(defun my/tide-hook ()
  "Custom Tide Hook."
  (tide-setup)
  (tide-hl-identifier-mode t)
  (prettier-js-mode t)
  (add-hook 'after-save-hook 'tide-format-before-save nil t)
  ;; (flycheck-add-next-checker 'typescript-tide 'javascript-eslint 'append)
  ;; (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)
  (with-eval-after-load 'evil
    (evil-define-key 'normal tide-mode-map
      (kbd "C-c o") 'tide-organize-imports
      (kbd "C-c r") 'tide-rename-symbol
      (kbd "C-c l") 'tide-add-eslint-disable-next-line
      (kbd "C-c TAB") 'tide-fix
      "gd" 'tide-jump-to-definition)))

(use-package tide
  :ensure t
  :init
  (add-hook 'typescript-mode-hook 'my/tide-hook))

;; Rust language support
(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (if (boundp 'rust-format-on-save) ;; XXX why is this check needed?
    (setq rust-format-on-save t)))

(use-package lsp-rust
  :after lsp-mode)

;; Dockerfile support
(use-package dockerfile-mode
  :mode "\\Dockerfile\\'"
  :ensure t)

;; Elm support
(use-package elm-mode
  :ensure t
  :mode "\\.elm\\'"
  :config
  (add-hook 'elm-mode-hook 'elm-format-on-save-mode))

;; Markdown support
(use-package markdown-mode
  :ensure t
  :mode (("\\.markdown\\'" . gfm-mode) ("\\.md\\'" . gfm-mode)))

;; PureScript support
(defun my/purescript-mode-hook ()
  "Custom PureScript Hook."
  (turn-on-purescript-indentation)
  ;; Define abbreviations
  (define-abbrev-table 'local-abbrev-table
    '(("forall" "∀" nil 1)))
  ;; Tabs/Spaces
  (setq-local indent-tabs-mode nil
              tab-width 2
              evil-shift-width 2))

(use-package purescript-mode
  :ensure t
  :mode "\\.purs\\'"
  :hook (purescript-mode . my/purescript-mode-hook))

;; PureScript IDE tooling
(defun my-psc-ide-hook ()
  "Custom PSC IDE Hook."
  (psc-ide-mode t)
  (setq psc-ide-use-npm-bin t)
  (company-mode t)
  (flycheck-mode t)
  (turn-on-purescript-indentation)
  (setq evil-shift-width 2)
  (with-eval-after-load 'evil
    (evil-define-key 'normal purescript-mode-map
      "gd" 'psc-ide-goto-definition)))

(use-package psc-ide
  :ensure t
  :commands (psc-ide-mode)
  :init (add-hook 'purescript-mode-hook 'my-psc-ide-hook))

;; Haskell support
(use-package haskell-mode
  :ensure t
  :init
  (setq haskell-process-type 'stack-ghci))

(defun my/lsp-haskell-hook ()
  "Custom LSP Haskell Integration Hook."
  (lsp-deferred))

(use-package lsp-haskell
  :ensure t
  :init
  (add-hook 'haskell-mode-hook  'my/lsp-haskell-hook))

;; JSON language support
(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)

;; Dhall language support
(use-package dhall-mode
  :ensure t
  :mode "\\.dhall\\'"
  :config
  (setq dhall-format-arguments '("--ascii")))

;; YAML language support
(use-package yaml-mode
  :ensure t
  :mode ("\\.yaml'" "\\.yml'")
  :config
  (add-hook 'yaml-mode-hook 'my/prog-mode-hook))

; ------------------------------------------------------------------------------
; Holidays
; ------------------------------------------------------------------------------

(eval-when-compile
  (require 'calendar)
  (require 'holidays))

;; Updated for: 2019
(defvar holiday-nz-holidays
  '((holiday-fixed  1  1 "New Year's Day")
    (holiday-fixed  1  2 "Day after New Year's Day")
    (holiday-fixed  2  6 "Waitangi Day")
    (holiday-fixed  4 19 "Good Friday")
    (holiday-fixed  4 22 "Easter Monday")
    (holiday-fixed  4 25 "ANZAC Day")
    (holiday-fixed  6  3 "Queen's Birthday")
    (holiday-fixed 10 28 "Labour Day")
    (holiday-fixed 12 25 "Christmas Day")
    (holiday-fixed 12 26 "Boxing Day"))
    "New Zealand holidays.")
(setq calendar-holidays holiday-nz-holidays)

;; -----------------------------------------------------------------------------
;; Org mode
;; -----------------------------------------------------------------------------
(defun my/org-mode-hook ()
  "Custom `org-mode' hook."
  (flyspell-mode t))

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  :pin org
  :config
  (setq org-directory "~/org"
        org-cycle-include-plain-lists 'integrate
        org-cycle-emulate-tab nil
        org-agenda-span 10
        org-agenda-files
        '("~/org/gtd.org"
          "~/org/tickler.org"
          "~/org/journal.org"
          "~/org/sylo.org")
        org-default-notes-file "~/org/gtd.org"
        org-agenda-custom-commands
        '(("o" "At the office" tags-todo "@office"
           ((org-agenda-overriding-header "Office")))
          ("h" "At home" tags-todo "@home"
           ((org-agenda-overriding-header "Home"))))
        org-capture-templates
        '(("i" "issue" entry
           (file+olp+datetree "~/org/issue-tracker.org")
           "* ONCE %?" :time-prompt t)))

    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c c") 'org-capture)

    (add-hook 'org-mode-hook 'my/org-mode-hook)

    ;; install org-bable languages
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((dot . t)))

    ;; re-display images after editing babel code blocks (i.e. dot graphs)
    (add-hook 'org-babel-after-execute-hook
              (lambda ()
                (when org-inline-image-overlays
                  (org-redisplay-inline-images))))

    ;; Prevent insertion of leading whitespace when hitting 'o' on a heading in
    ;; org-mode.
    ;; See: https://github.com/syl20bnr/spacemacs/issues/11204
    (setq org-adapt-indentation nil))

;; Evil support for org-mode
(defun my/evil-org-mode-hook ()
  "Custom `evil-org-mode' hook."
  (evil-org-set-key-theme '(navigation
                            insert
                            textobjects
                            additional
                            calendar
                            return)))
(use-package evil-org
  :ensure t
  :after (evil org)
  :config
  (evil-org-set-key-theme
   '(navigation insert textobjects additional calendar))
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook 'my/evil-org-mode-hook))

;; Graphviz support
(defun my/graphviz-dot-preview ()
  "Regenerate graphviz image.
The image file name is derived from the name of the dot file.
Use this with 'eog' to get live reload."
    (interactive)
    (save-buffer)
    (let ((windows (window-list))
          (f-name (graphviz-output-file-name (buffer-file-name)))
          (warn-msg (string-trim (shell-command-to-string compile-command))))
      (if (string-match-p "^Warning: .+ line \\([0-9]+\\)" warn-msg)
          (message warn-msg))))
(use-package graphviz-dot-mode
  :ensure t
  :config
  (add-hook 'after-save-hook 'my-graphviz-dot-preview nil t))

; ------------------------------------------------------------------------------

(provide 'init)

;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(sql-indent helm-rg irony-mode company-irony irony clang-format go-mode swiper prettier-js prettier-eslint undo-tree protobuf-mode kotlin-mode cmake-mode lsp-java use-package ace-window))
 '(safe-local-variable-values
   '((python-shell-process-environment "DJANGO_SETTINGS_MODULE=vho_shop.settings" "CODOH_SHOP_DEBUG=True" "CODOH_SHOP_NP_API_KEY=bogus" "CODOH_SHOP_NP_GATEWAY_URL=bogus" "CODOH_SHOP_SQUAREUP_APPLICATION_ID=bogus" "CODOH_SHOP_SQUAREUP_TOKEN=bogus" "CODOH_SHOP_SQUAREUP_LOCATION_ID=bogus" "CODOH_SHOP_SITE_URL=http://localhost:8000")
     (python-shell-extra-pythonpaths "/home/felix/dev/codoh/shop.codoh.com")
     (eval let
	   ((root
	     (projectile-project-root)))
	   (setq-local company-clang-arguments
		       (list
			(concat "-I" root "src")
			(concat "-I" root "include")
			(concat "-I" root "build/host/debug")))
	   (setq-local flycheck-clang-language-standard "c++11" flycheck-clang-include-path
		       (list
			(concat root "src")
			(concat root "include")
			(concat root "build/host/debug"))))
     (eval let
	   ((root
	     (projectile-project-root)))
	   (setq-local company-clang-arguments
		       (list
			(concat "-I" root "src")
			(concat "-I" root "include")
			(concat "-I" root "build/host")))
	   (setq-local flycheck-clang-include-path
		       (list
			(concat root "src")
			(concat root "include")
			(concat root "build/host"))))
     (eval let
	   ((root
	     (projectile-project-root)))
	   (setq-local company-clang-arguments
		       (list
			(concat "-I" root "src")
			(concat "-I" root "include")
			(concat "-I" root "build/host")))
	   (setq-local flycheck-clang-include-path
		       (list
			(concat "-I" root "src")
			(concat "-I" root "include")
			(concat "-I" root "build/host"))))
     (web-mode-engines-alist
      ("django" . "\\.html\\'")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
