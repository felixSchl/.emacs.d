(package-initialize)

(add-to-list 'load-path "~/.emacs.d/use-package")
(eval-when-compile
  (require 'use-package))

;; ------------------
;; Package management
;; ------------------

;; Please don't load outdated byte code
(setq load-prefer-newer t)

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
    (package-install 'use-package))

;; --------
;; Settings
;; --------

;; Standard stuff
(line-number-mode)
(column-number-mode)

;; ------------------
;; Configure Packages
;; ------------------

;; Evil - VIM emulation layer for emacs
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
