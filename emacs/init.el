(setq lexical-binding t)

;; Display start-up-time
(defun my/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                     (time-subtract after-init-time before-init-time)))
           gcs-done))

(add-hook 'emacs-startup-hook #'my/display-startup-time)

;; Bootstrap straight.el during first load
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5)) (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))

;; Install use-package for rest of configuration
(straight-use-package 'use-package)

                                        ; Package Management
(use-package straight
  :custom (straight-use-package-by-default t))

                                        ; Dotfiles Cleanup
(use-package no-littering
  :init
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "back-up/")))
        lock-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "lock-files/") t))
        custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage))

                                        ; Startup Profiling
(use-package esup
  :custom
  (esup-user-init-file (file-truename "~/.config/emacs/init.el")))

                                        ; Vi-Emulation
(use-package goto-chg)

(use-package evil
  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-disable-insert-state-bindings t)

  :init
  (setq evil-insert-state-cursor 'bar
        evil-motion-state-cursor 'hbar
        evil-operator-state-cursor 'hbar)

  :config
  (evil-mode 1)

  (evil-set-leader '(normal visual operator motion) (kbd "SPC")))

                                        ; Mapping Enhancements
(use-package general
  :config
  (general-define-key :states 'motion
    "s-<left>" 'back-to-indentation
    "H" 'back-to-indentation
    "s-<right>" 'evil-end-of-line
    "L" 'evil-end-of-line
    "Q" 'evil-window-delete))

                                        ; Emacs Configuration
(use-package emacs
  :straight nil
  :no-require
  :defer nil
  :preface
  ;; Delete word instead of kill word
  (defun my/delete-word (arg)
    "Delete characters forward until encountering the end of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
    (interactive "p")
    (delete-region (point) (progn (forward-word arg) (point))))

  (defun my/backward-delete-word (arg)
    "Delete characters backward until encountering the beginning of a word.
With argument, do this that many times.
This command does not push text to `kill-ring'."
    (interactive "p")
    (my/delete-word (- arg)))

  :custom
  ;; preserve system clipboard when using kill-ring
  (select-enable-clipboard nil)

  (tab-always-indent 'complete)

  ;; always open help buffers in existing help window
  (display-buffer-base-action
   `((display-buffer-reuse-mode-window
      display-buffer-use-some-window
      display-buffer-same-window)
     . ((mode . (help-mode helpful-mode apropos-mode)))))

  :init
  ;; default to disabling using tabs for indents
  (setq-default indent-tabs-mode nil)

  :general
  ;; keybinds for clipboard copy/paste
  ("s-v" 'clipboard-yank)
  ("s-c" 'clipboard-kill-ring-save)

  ;; remap deletion keybinds
  ("<M-backspace>" 'my/backward-delete-word)
  ("M-d" 'my/delete-word)

  :config
  ;; refresh buffers if files were changed on disk (unless unsaved)
  (global-auto-revert-mode 1))

                                        ; History
(use-package savehist
  :custom (history-length 25)
  :config (savehist-mode 1))

(use-package recentf
  :config (recentf-mode 1)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))

                                        ; Aesthetics
;; visual cleanup
(use-package visual-cleanup
  :straight nil
  :no-require
  :preface
  (defun my/flash-mode-line ()
    (invert-face 'mode-line)
    (run-with-timer 0.1 nil #'invert-face 'mode-line))

  :custom
  (visible-bell nil)
  (ring-bell-function 'my/flash-mode-line)

  :config
  (blink-cursor-mode -1))

;; cursorline
(use-package hl-line-mode
  :straight nil
  :hook (prog-mode text-mode)
  :custom (hl-line-sticky-flag nil))

;; line numbers
(use-package display-line-numbers-mode
  :straight nil
  :hook (prog-mode text-mode))

;; show whitespace in prog-mode
(use-package whitespace-mode
  :straight nil
  :hook prog-mode
  :custom
  (whitespace-style '(face trailing tabs space-after-tab space-before-tab)))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t))

(use-package all-the-icons)

(use-package doom-modeline
  :after all-the-icons
  :custom
  (doom-modeline-unicode-fallback t)
  :config
  (column-number-mode 1)
  (doom-modeline-mode 1))

(use-package solaire-mode
  :config
  (solaire-global-mode 1))

(use-package evil-quickscope
  :hook ((prog-mode text-mode) . turn-on-evil-quickscope-always-mode)
  :custom-face
  (evil-quickscope-first-face ((t (:inherit 'font-lock-constant-face :underline t :bold t))))
  (evil-quickscope-second-face ((t (:inherit 'font-lock-type-face :underline t :bold t)))))

                                        ; UI Enhancements
(use-package helpful
  :general
  ("C-h f" 'helpful-callable)
  ("C-h x" 'helpful-command)
  ("C-h v" 'helpful-variable)
  ("C-h k" 'helpful-key)
  ("C-h F" 'helpful-function)
  ("C-c C-d" 'helpful-at-point)

  (:keymaps 'helpful-mode-map :states 'normal
            "q" 'quit-window))

(use-package which-key
  :custom
  (which-key-show-early-on-C-h t)
  :config
  (which-key-mode 1))

(use-package popon
  :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon")
  :if (not (display-graphic-p)))

                                        ; Terminal Replacement
(use-package vterm
  :custom (vterm-always-compile-module t)
  :general
  (:keymaps 'vterm-mode-map :states 'insert
            "C-u" 'vterm-send-C-u
            "C-d" 'vterm-send-C-d))

                                        ; Editing Enhancements
(use-package evil-surround
  :hook ((prog-mode text-mode) . evil-surround-mode)
  :config
  (setq-default evil-surround-pairs-alist (push '(?< . ("< " . " >")) evil-surround-pairs-alist)
                evil-surround-pairs-alist (push '(?> . ("<" . ">")) evil-surround-pairs-alist)))

(use-package evil-nerd-commenter
  :general
  (:states 'normal "<leader>c" 'evilnc-comment-operator)
  (:states 'visual "<leader>c" 'evilnc-comment-or-uncomment-lines))

;; autopairs
(use-package electric-pair
  :straight nil
  :no-require
  :config (electric-pair-mode 1))

                                        ; Navigation Enhancements
(use-package better-jumper
  :general
  (:states 'motion
           "C-o" 'better-jumper-jump-backward
           "C-i" 'better-jumper-jump-forward)
  :config
  (better-jumper-mode 1))

(use-package evil-visualstar
  :config (global-evil-visualstar-mode 1))

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-keys '(?s ?l ?a ?g ?h ?v ?m ?e ?i ?r ?u ?w ?o ?c ?x ?d ?k ?f ?j))
  (avy-orders-alist '((avy-goto-char-2 . avy-order-closest)
                      (avy-goto-line . avy-order-closest)))
  :general
  (:states 'motion
           "<leader>j" 'evil-avy-goto-line-below
           "<leader>k" 'evil-avy-goto-line-above)
  (:states '(normal visual operator motion) "s" 'evil-avy-goto-char-2))

(use-package projectile
  :general
  (:states 'normal
           "<leader>ff" 'projectile-find-file
           "<leader>fg" 'projectile-find-file-dwim

           "<leader>fp" 'project-switch-project

           "<leader>FF" 'projectile-find-file-in-directory)
  :config
  (projectile-mode 1))

                                        ; Window Management
(use-package windmove
  :custom (windmove-create-window t)
  :commands (windmove-left windmove-down windmove-up windmove-right)
  :general
  (:states 'normal
           "S-<left>" 'windmove-left
           "S-<down>" 'windmove-down
           "S-<up>" 'windmove-up
           "S-<right>" 'windmove-right))

                                        ; Version Control
(use-package diff-hl
  :hook
  ((prog-mode text-mode vc-dir-mode) . turn-on-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  :general
  (:states 'motion
           "]c" 'diff-hl-next-hunk
           "[c" 'diff-hl-previous-hunk
           "<leader>hs" 'diff-hl-stage-current-hunk
           "<leader>hU" 'diff-hl-unstage-file
           "<leader>hr" 'diff-hl-revert-hunk))

                                        ; Completion Enhancements
(use-package vertico
  :preface
  (defun my/crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'my/crm-indicator)
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  :custom
  (vertico-cycle t)
  (vertico-count 20)

  ;; hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

  :config
  (vertico-mode 1))

(use-package orderless
  :preface
  (defun my/match-components-literally ()
    "Components match literally for the rest of this minibuffer session."
    (interactive)
    (setq-local orderless-matching-styles 'orderless-literal
                orderless-style-dispatchers nil))

  (general-define-key :keymaps 'vertico-map "C-l" 'my/match-components-literally)

  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-prefixes orderless-flex))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion flex)))))

(use-package marginalia
  :after vertico
  :config (marginalia-mode 1))

(use-package consult
  :general
  (:states 'normal
           "<leader>fb" 'consult-buffer
           "<leader>/" 'consult-line

           "<leader>fsr" 'consult-ripgrep
           "<leader>fhs" 'consult-recent-file))

(use-package corfu
  :defer nil

  :preface
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer 1)

  ;; Quit Corfu after exiting insert mode
  :hook (evil-insert-state-exit . corfu-quit)

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)

  :general
  (:keymaps 'corfu-map
            "RET" nil
            "M-S-SPC" 'corfu-insert-separator)

  :config
  (global-corfu-mode 1))

(use-package corfu-terminal
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal")
  :requires (corfu popon)
  :if (not (display-graphic-p))
  :config (corfu-terminal-mode 1))

(use-package corfu-doc
  :requires corfu
  :hook (corfu-mode . corfu-doc-mode)
  :general
  (:keymaps 'corfu-map
            "M-d" 'corfu-doc-toggle
            "M-p" 'corfu-doc-scroll-down
            "M-n" 'corfu-doc-scroll-up))

(use-package corfu-doc-terminal
  :straight (corfu-doc-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal")
  :requires (corfu-doc popon)
  :if (not (display-graphic-p))
  :config (corfu-doc-terminal-mode 1))
