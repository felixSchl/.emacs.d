(setq load-prefer-newer t)

;; ------------------
;; Package management
;; ------------------

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
    (package-install 'use-package))

(eval-when-compile
    (require 'use-package))

;; --------
;; Settings
;; --------

(line-number-mode)
(column-number-mode)
(setq inhibit-splash-screen t)
(savehist-mode t)

;; Remap command to option on Apple Mac
(setq mac-command-modifier 'super)
(setq mac-option-modifier 'meta)
;; ------------------
;; Theme
;; ------------------

(use-package atom-one-dark-theme
  :ensure t
  :init
  (load-theme
   (if (display-graphic-p)
       'atom-one-dark
     'tsdh-dark)
   t))

;; ------------------
;; Configure Packages
;; ------------------

;; Key-chord - Key stroke combos
(use-package key-chord
  :ensure t
  :init
  (key-chord-mode t))

;; Evil - VIM emulation layer for emacs
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)
(use-package evil
  :ensure t
  :init
  (evil-mode t)
  ;; The fastest way to leave insert mode:
  (dolist (x '("jk" "jK" "JK" "Jk" "kj" "kJ" "KJ" "Kj"))
    (key-chord-define evil-insert-state-map x 'evil-normal-state)))

;; Magit - A vim porcelain
(use-package magit
  :ensure t)

;; Company - Auto-completion
(use-package company
  :ensure t
  :init
  (global-company-mode)
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t))

;; Spaceline - A mode line
(use-package spaceline
  :ensure t
  :init
  (require 'spaceline-config)
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state)
  (spaceline-spacemacs-theme))

;; Paredit-compat mode
(use-package evil-paredit
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook 'evil-paredit-mode))
