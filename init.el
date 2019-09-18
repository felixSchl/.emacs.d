(setq load-prefer-newer t)

;; -----------------------------------------------------------------------------
;; Package management ----------------------------------------------------------
;; -----------------------------------------------------------------------------

(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("emacs-pe" . "https://emacs-pe.github.io/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/vendor")

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; -----------------------------------------------------------------------------
;; Settings --------------------------------------------------------------------
;; -----------------------------------------------------------------------------

(use-package diminish)

(setq evil-want-keybinding nil)
(line-number-mode)
(column-number-mode)
(setq inhibit-splash-screen t)
;(smooth-scrolling-mode)
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

;; Configure dired-mode
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-omit-mode)))

;; Define abbreviations
(define-abbrev-table 'global-abbrev-table
  '(("forall" "∀" nil 1)))

;; Auto-revert dired buffers when files change on disk
(add-hook 'dired-mode-hook
          (lambda ()
            (auto-revert-mode t)))

;; Toggle-Maximize a buffer
(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))
(global-set-key [(meta shift return)] 'toggle-maximize-buffer)

;; Configure Javascript
(setq js-indent-level 2)

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

;; Always save the buffer (even if emacs thinks there are no changes)
(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))
(global-set-key (kbd "C-x C-s") 'save-buffer-always)

(defun --prog-mode-hook ()
  (setq indent-tabs-mode nil)
  (show-paren-mode t)
  (set-fill-column 80)
  (abbrev-mode t)
  (abbrev-mode t)
  (fci-mode t)
  (setq indent-tabs-mode nil)
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
;; Org mode --------------------------------------------------------------------
;; -----------------------------------------------------------------------------

(use-package org
  :mode (("\\.org$" . org-mode))
  :ensure t
  :pin org
  :config
  (progn
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
            '(("i" "Inbox" entry
               (file+headline "~/org/gtd.org" "Inbox")
               "* TODO %i%?")
              ("t" "Tickler" entry
               (file+headline "~/org/tickler.org" "Tickler")
               "* %i%? \n %U")))

    (global-set-key (kbd "C-c l") 'org-store-link)
    (global-set-key (kbd "C-c a") 'org-agenda)
    (global-set-key (kbd "C-c c") 'org-capture)

    ;; Prevent insertion of leading whitespace when hitting 'o' on a heading in
    ;; org-mode.
    ;; See: https://github.com/syl20bnr/spacemacs/issues/11204
    (setq org-adapt-indentation nil)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))

(eval-after-load 'org
  (lambda ()
    (add-hook 'org-babel-after-execute-hook
              (lambda ()
                (when org-inline-image-overlays
                    (org-redisplay-inline-images))))
    (add-hook 'org-mode-hook
              (lambda ()
                (flyspell-mode t)
                (setq default-fill-column 80)))))

;; -----------------------------------------------------------------------------
;; Configure Packages ----------------------------------------------------------
;; -----------------------------------------------------------------------------

(use-package graphviz-dot-mode
  :ensure t
  :config
  (progn
    (defun my-graphviz-dot-preview ()
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
    (defun my-graphviz-dot-mode-hook ()
      (add-hook 'after-save-hook 'my-graphviz-dot-preview))
    (add-hook 'graphviz-dot-mode-hook 'my-graphviz-dot-mode-hook)))

;; Integrate with `keychain` utility
(use-package keychain-environment
  :ensure t)

(use-package git-link
  :ensure t
  :config
  (progn
    (setq git-link-use-commit t)))

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
    (push '("*undo-tree*"
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

(use-package monokai-theme
  :ensure t
  :init '(load-theme 'monokai t))

(use-package fill-column-indicator
  :ensure t)

;; Recover the $PATH from the shell
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Smooth-Scrolling - Retain context during scrolling
(use-package smooth-scrolling
  :ensure t
  :config
    (setq scroll-margin 1
      scroll-conservatively 0
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
    (setq-default scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01))

;; Flycheck - on-the-fly syntax checking
(use-package flycheck
  :ensure t
  :init
  (add-hook 'after-init-hook
            (lambda()
              (global-flycheck-mode)
              (diminish 'flycheck-mode)))
  :config
  (setq flycheck-check-syntax-automatically '(save)))

;; Key-chord - Key stroke combos
(use-package key-chord
  :ensure t
  :config
  (key-chord-mode t))

;; Evil - VIM emulation layer for emacs
(use-package evil
  :ensure t
  :init
   (setq evil-respect-visual-line-mode t)
   (setq evil-want-C-i-jump t)
   (setq evil-want-C-u-scroll t)
  :config
  (evil-mode t)
  (diminish 'undo-tree-mode)

  ;; Do not consider saving a buffer a repeatable action
  (evil-declare-not-repeat 'custom-save-buffer)

  ;; Return to normal mode when saving files using C-X C-S
  (add-hook 'after-save-hook #'evil-normal-state)

  ;; "Hybrid" editing style:
  ;; (this allows for keymacs key-bindings in insert-mode)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  ;; flip c-v and v visual modes
  (define-key evil-normal-state-map (kbd  "v") 'evil-visual-block)
  (define-key evil-normal-state-map (kbd "C-v") 'evil-visual-char)
  (define-key evil-visual-state-map (kbd "v") 'evil-visual-block)
  (define-key evil-visual-state-map (kbd "C-v") 'evil-visual-char)

  ;; Do not use vim mappings in some modes
  (evil-set-initial-state 'term-mode 'emacs)
  (evil-set-initial-state 'git-rebase-mode 'emacs)

  ;; The fastest way to leave insert mode:
  (dolist (x '("jk" "jK" "JK" "Jk" "kj" "kJ" "KJ" "Kj"))
    (key-chord-define evil-insert-state-map x 'evil-normal-state))

  ;; Start git commit mode in evil insert mode
  (add-hook 'git-commit-mode-hook 'evil-insert-state)

  ;; Mimic fugitive bindings
  (evil-ex-define-cmd "Gst[atus]" 'magit-status))

(defun custom-save-buffer ()
  "Save the buffer but don't muck up evil-repeat."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer nil))

;; Evil support for org-mode
(use-package evil-org
  :ensure t
  :requires evil
  :after org
  :config
  (evil-org-set-key-theme '(navigation insert textobjects additional calendar))
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme '(navigation
                                        insert
                                        textobjects
                                        additional
                                        calendar
                                        return))))
  (global-set-key (kbd "C-x C-s") 'custom-save-buffer))

;; Surround mode
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode t))

;; Tree-based directory browsing
(use-package dired-subtree
  :ensure t
  :init
  (eval-after-load 'dired
    '(progn
       (evil-make-overriding-map dired-mode-map 'normal t)
       (evil-define-key 'normal dired-mode-map
         "g" 'revert-buffer)
       (evil-define-key 'normal dired-mode-map
         "o" 'dired-subtree-toggle))))

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
  (global-set-key (kbd "C-x g") 'magit-status)
  (setq magit-diff-paint-whitespace t)
  (setq magit-diff-highlight-trailing t))

;; Company - Auto-completion
(use-package company
  :ensure t
  :init
  (setq company-selection-wrap-around t)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil)
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
  (progn
    (spaceline-define-segment buffer-id
      (if (buffer-file-name)
          (let ((project-root (projectile-project-p)))
            (if project-root
                (file-relative-name (buffer-file-name) project-root)
              (abbreviate-file-name (buffer-file-name))))
        (powerline-buffer-id)))
    (spaceline-spacemacs-theme)
    (spaceline-toggle-minor-modes-off)))

;; Diff-hl - Highlight changed lines
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode)
  (unless (display-graphic-p)
    (setq diff-hl-side 'left)
    (diff-hl-margin-mode)))

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

;; A better git-grep experience than the one built into helm
(use-package helm-git-grep
  :ensure t
  :config
  (global-set-key (kbd "C-c g f") 'helm-git-grep)
  (global-set-key (kbd "C-c C-f") 'helm-git-grep))

(use-package helm-ls-git
  :ensure t)

;; Projectile - Project interaction library
(use-package projectile
  :ensure t
  :config
  (projectile-mode t)
  (setq projectile-switch-project-action 'projectile-dired)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
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

;; Eyebrowse - Multiple window configurations
(use-package eyebrowse
  :ensure t
  :config
  (eyebrowse-mode 1)
  (let ((map eyebrowse-mode-map))
    (define-key map (kbd "M-0") 'eyebrowse-switch-to-window-config-0)
    (define-key map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
    (define-key map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
    (define-key map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
    (define-key map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
    (define-key map (kbd "M-5") 'eyebrowse-switch-to-window-config-5)
    (define-key map (kbd "M-6") 'eyebrowse-switch-to-window-config-6)
    (define-key map (kbd "M-7") 'eyebrowse-switch-to-window-config-7)
    (define-key map (kbd "M-8") 'eyebrowse-switch-to-window-config-8)
    (define-key map (kbd "M-9") 'eyebrowse-switch-to-window-config-9)))

;; Use nlinum mode over normal linum mode
(use-package nlinum
  :ensure t
  :config
  (add-hook 'prog-mode-hook
            (lambda ()
              (nlinum-mode t))))

(defun setup-mu4e ()
  (require 'mu4e)
  (add-hook 'mu4e-view-mode-hook 'visual-line-mode)
  (setq
   mu4e-compose-dont-reply-to-self t
   mu4e-maildir "~/mail"
   mu4e-html2text-command "html2markdown"
   mu4e-headers-show-threads t
   mu4e-update-interval 100
   mu4e-hide-index-messages t
   mu4e-user-mail-address-list
   '("felixschlitter@gmail.com" "felixschlitter@fastmail.com")
   mu4e-sent-messages-behavior 'sent
   message-send-mail-function 'smtpmail-send-it
   mu4e-get-mail-command "mbsync gmail-inbox gmail-drafts fastmail-inbox fastmail-drafts"
   mu4e-change-filenames-when-moving t
   mu4e-contexts
   `( ,(make-mu4e-context
        :name "Gmail"
        :match-func
        (lambda (msg)
          (when msg
            (mu4e-message-contact-field-matches
             msg :to "felixschlitter@gmail.com")))
        :vars '( ( user-mail-address      . "felixschlitter@gmail.com"  )
                 ( user-full-name         . "Felix Schlitter" )
                 ( mu4e-sent-folder       . "/gmail/Sent" )
                 ( mu4e-drafts-folder     . "/gmail/Drafts" )
                 ( mu4e-trash-folder      . "/gmail/Trash" )
                 ( mu4e-refile-folder     . "/gmail/Archive" )
                 ( smtpmail-smtp-user     . "felixschlitter@gmail.com" )
                 ( smtpmail-smtp-server   . "smtp.gmail.com" )
                 ( smtpmail-smtp-service  . 587 )
                 ( mu4e-compose-signature . nil )))
      ,(make-mu4e-context
        :name "Fastmail"
        :match-func
        (lambda (msg)
          (when msg
            (mu4e-message-contact-field-matches
             msg :to "felixschlitter@fastmail.com")))
        :vars '( ( user-mail-address       . "felixschlitter@fastmail.com" )
                 ( user-full-name          . "Felix Schlitter" )
                 ( mu4e-sent-folder        . "/fastmail/Sent" )
                 ( mu4e-drafts-folder      . "/fastmail/Drafts" )
                 ( mu4e-trash-folder       . "/fastmail/Trash" )
                 ( mu4e-refile-folder      . "/fastmail/Archive" )
                 ( smtpmail-smtp-user      . "felixschlitter@fastmail.com" )
                 ( smtpmail-smtp-server    . "smtp.fastmail.com" )
                 ( smtpmail-smtp-service   . 587 )
                 ( mu4e-compose-signature  . nil )))))
  (add-to-list 'mu4e-view-actions
               '("ViewInBrowser" . mu4e-action-view-in-browser) t)
  ;; mu4e alerts
  (use-package mu4e-alert
    :ensure t
    :config
    (mu4e-alert-enable-mode-line-display)
    (setq mu4e-alert-interesting-mail-query
          "(maildir:/gmail/Inbox OR maildir:/fastmail/Inbox) AND flag:unread"))
  (setq my-mu4e-has-loaded 1))

(setq my-mu4e-has-loaded nil)
(if (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e")
    (progn
      (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
      (setup-mu4e)))

;; A set of useful evil bindings
(use-package evil-collection
  :ensure t
  :requires evil
  :config
  (evil-collection-calendar-setup)
  (evil-collection-term-setup)
  (setq evil-collection-outline-bind-tab-p t)
  (evil-collection-outline-setup)
  (evil-collection-magit-setup)
  (if my-mu4e-has-loaded
    (evil-collection-mu4e-setup)))

;; -----------------------------------------------------------------------------
;; Vendored packages
;; -----------------------------------------------------------------------------

(require 'taskjuggler-mode)

;; (TODO) generalize this, currently the time format used is specific to tj3.
(defun calendar-insert-date ()
  "Capture the date at point, exit the Calendar, insert the date."
  (interactive)
  (seq-let (month day year) (save-match-data (calendar-cursor-to-date))
    (calendar-exit)
    (insert (format "%d-%02d-%02d" year month day))))
(evil-define-key 'normal calendar-mode-map
  (kbd "RET") 'calendar-insert-date)

(defun my-taskjuggler-helm-tasks  ()
  (interactive)
  (let ((task-tree (taskjuggler-parser nil 'tasktree nil)))
    (helm :sources
            (helm-build-in-buffer-source "taskjuggler-tasks"
              :action (lambda (line)
                        (pcase (split-string line ":")
                          (`(,n ,_)
                           (with-no-warnings
                             (goto-line (string-to-number n))))))
              :data (seq-filter 'identity
                                (mapcar (lambda(item)
                                          (pcase item
                                            (`(,path ,_ ,_ ,name ,line)
                                             (concat (number-to-string line) ": " path " " name))))
                                        task-tree))))))

(defun my-task-juggler-mode-hook ()
  (interactive)
  (hs-minor-mode t)
  (setq-local hs-allow-nesting 1)
  (evil-define-key 'insert taskjuggler-mode-map
    (kbd "C-c n") (lambda ()
                    (interactive)
                    (let ((stamp (format-time-string "%Y-%m-%d-%H:%M" (current-time))))
                      (insert stamp))))

  (evil-define-key 'normal taskjuggler-mode-map
    (kbd ", v") (lambda ()
                  (interactive)
                  (let ((path (taskjuggler-current-context-path)))
                    (kill-new path)
                    (message path)))
    (kbd ", n") (lambda ()
                  (interactive)
                  (let ((stamp (format-time-string "%Y-%m-%d-%H:%M" (current-time))))))
    (kbd "C-c C-j") 'my-taskjuggler-helm-tasks
    (kbd "<backtab>") 'hs-hide-level
    (kbd "<tab>") (lambda ()
                    (interactive)
                    (let ((evil-cross-lines 0))
                         (evil-beginning-of-line))
                    (ignore-errors
                      (evil-find-char 1 ?{))
                    (hs-toggle-hiding))))

(add-hook 'taskjuggler-mode-hook 'my-task-juggler-mode-hook)
(add-to-list 'hs-special-modes-alist
             '(taskjuggler-mode "{" "}" "/[*/]" nil))

;; -----------------------------------------------------------------------------
;; Extras
;; -----------------------------------------------------------------------------

(defun pin-buffer ()
  "Pin buffer to current window."
  (interactive)
  (message
   (if (let (window (get-buffer-window (current-buffer)))
         (set-window-dedicated-p window (not (window-dedicated-p window))))
       "pinned buffer" "un-pinned buffer")
   ))

;; -----------------------------------------------------------------------------
;; Language / Framework support-------------------------------------------------
;; -----------------------------------------------------------------------------

;; PostgreSQL
(defun pgsql-scratch ()
  (interactive)
  (switch-to-buffer "*pgsql-scratch*")
  (sql-mode)
  (sql-set-product "postgres")
  (sql-set-sqli-buffer))

;; MySQL
(defun mysql-scratch ()
  (interactive)
  (switch-to-buffer "*mysql-scratch*")
  (sql-mode)
  (sql-set-product "mysql")
  (sql-set-sqli-buffer))

;; asciidoc
(use-package adoc-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist (cons "\\.adoc\\'" 'adoc-mode))
    (defun my-adoc-mode-hook ()
      ;; (buffer-face-mode t)

      (flyspell-mode)

      (face-remap-add-relative
       'markup-title-1-face
       `(:foreground ,(face-foreground 'org-level-1)))

      (face-remap-add-relative
       'markup-title-2-face
       `(:foreground ,(face-foreground 'org-level-2)))

      (face-remap-add-relative
       'markup-title-3-face
       `(:foreground ,(face-foreground 'org-level-3)))

      (face-remap-add-relative
       'markup-title-4-face
       `(:foreground ,(face-foreground 'org-level-4)))

      (face-remap-add-relative
       'markup-meta-hide-face
       `(:foreground ,(face-foreground 'markup-meta-face)))
      (face-remap-add-relative
       'markup-table-face
       `(:foreground ,(face-foreground 'markup-meta-face)
         :background ,(face-background 'default)))
      (face-remap-add-relative
        'markup-anchor-face
        '(:overline nil))
      (face-remap-add-relative
       'markup-list-face
       `(:background ,(face-background 'default)
         :foreground ,(face-foreground 'markup-meta-face))))

    (add-hook 'adoc-mode-hook 'my-adoc-mode-hook)
    (evil-define-key 'normal adoc-mode-map
      (kbd "<backtab>") 'outline-show-all
      (kbd "<tab>") 'outline-toggle-children
      "zB" 'outline-hide-body
      "zb" 'outline-hide-entry
      "ze" 'outline-show-entry
      "zl" 'outline-hide-leaves
      "zK" 'outline-show-branches
      "zk" 'outline-show-children
      "zp" 'outline-hide-other
      "[" 'outline-previous-visible-heading
      "]" 'outline-next-visible-heading
      (kbd "C-k") 'outline-backward-same-level
      (kbd "C-j") 'outline-forward-same-level
      "gk" 'outline-backward-same-level
      "gj" 'outline-forward-same-level
      "^" 'outline-up-heading
      (kbd "M-h") 'outline-promote
      (kbd "M-j") 'outline-move-subtree-down
      (kbd "M-k") 'outline-move-subtree-up
      (kbd "M-l") 'outline-demote
      (kbd "M-<return>") 'outline-insert-heading)))

;; C
(defun --c-mode-hook ()
   (setq
    backward-delete-char-untabify-method nil
    c-basic-offset 8
    tab-width 8
    indent-tabs-mode t))

(add-hook
 'c-mode-common-hook
 '--c-mode-hook)

;; JSON
(use-package json-mode
  :ensure t)

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
            (tide-setup)
            (define-key evil-normal-state-map "gd" 'tide-jump-to-definition))))

;; Dockerfiles
(use-package dockerfile-mode
  :ensure t)

;; Elm
(use-package elm-mode
  :ensure t
  :config
  (elm-format-on-save-mode t))

;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq haskell-process-type 'stack-ghci)
              (setq evil-shift-width 2))))

;; Rust
(use-package rust-mode
    :mode "\\.rs\\'"
    :init
    (setq rust-format-on-save t))

(use-package lsp-rust
    :after lsp-mode)

;; Haskell
(defun my-haskell-mode-hook ()
  (intero-mode t)
  (define-key evil-normal-state-map "gd" 'intero-goto-definition))

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'my-haskell-mode-hook))

;; Purescript
(defun my-purescript-mode-hook ()
  (turn-on-purescript-indentation)
  (setq evil-shift-width 2))

(use-package purescript-mode
  :ensure t
  :config
  (add-hook 'purescript-mode-hook 'my-purescript-mode-hook))

(defun my-psc-ide-hook ()
  (setq psc-ide-use-npm-bin t)
  (psc-ide-mode t)
  (company-mode t)
  (flycheck-mode t)
  (turn-on-purescript-indentation)
  (setq evil-shift-width 2)
  (define-key evil-normal-state-map "gd" 'psc-ide-goto-definition))

(use-package psc-ide
  :ensure t
  :config
  (add-hook 'purescript-mode-hook 'my-psc-ide-hook))

;; Apple Swift
(use-package swift-mode
  :ensure t
  :config
  '(setq swift-mode:basic-offset 2))

(use-package flycheck-swift
  :ensure t
  :config
  '(eval-after-load 'flycheck '(flycheck-swift-setup)))

;; Shell
(add-hook 'sh-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (rainbow-mode yaml-mode xclip ws-butler use-package tide swift-mode spaceline smooth-scrolling purescript-mode psc-ide projectile popwin nlinum multi-term mu4e-alert monokai-theme markdown-mode magit keychain-environment key-chord json-mode intero ido-vertical-mode helm-ls-git helm-git-grep helm-flx graphviz-dot-mode go-mode git-link flycheck-swift flx-ido fill-column-indicator eyebrowse exec-path-from-shell evil-visualstar evil-surround evil-org evil-commentary evil-collection elm-mode dockerfile-mode dired-subtree dired-filter diminish diff-hl cmake-mode adoc-mode ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
