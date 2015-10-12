(setq load-prefer-newer t)

;; ------------------
;; Package management
;; ------------------

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
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

;; ------------------
;; Configure Packages
;; ------------------

;; Evil - VIM emulation layer for emacs
(setq evil-want-C-i-jump t)
(setq evil-want-C-u-scroll t)
(use-package evil
  :ensure t
  :init
  (evil-mode t)
  (defun insert-jay ()
    (interactive)
    (insert "j"))
  (define-key evil-insert-state-map (kbd "jk") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "jj") 'insert-jay))

;; Magit - A vim porcelain
(use-package magit
  :ensure t)
