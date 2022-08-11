(setq lexical-binding t)

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

                                        ; Emacs Configuration
(use-package emacs
  :straight nil
  :no-require
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

  ;; always open help buffers in new window and don't use an existing window
  (display-buffer-base-action
   `((display-buffer-reuse-mode-window
      display-buffer-in-side-window
      display-buffer-same-window)
     . ((mode . (help-mode helpful-mode apropos-mode))
        (side . right)
        (window-width . 0.5))))

  :init
  ;; default to disabling using tabs for indents
  (setq-default indent-tabs-mode nil)

  :config
  ;; keybinds for clipboard copy/paste
  (global-set-key (kbd "s-v") 'clipboard-yank)
  (global-set-key (kbd "s-c") 'clipboard-kill-ring-save)

  ;; remap deletion keybinds
  (global-set-key (kbd "<M-backspace>") 'my/backward-delete-word)
  (global-set-key (kbd "M-d") 'my/delete-word)

  ;; refresh buffers if files were changed on disk (unless unsaved)
  (global-auto-revert-mode 1))

                                        ; Package Management
(use-package straight
  :custom (straight-use-package-by-default t))

(use-package no-littering
  :init
  (setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
        backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "back-up/")))
        lock-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "lock-files/") t))
        custom-file (locate-user-emacs-file "custom-vars.el"))
  (load custom-file 'noerror 'nomessage))

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
  (inhibit-startup-message t)
  (cursor-type 'box)

  :init
  (set-face-attribute 'default nil :family "MesloLGS Nerd Font Mono" :height 160)

  :config
  (blink-cursor-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)

  (fringe-mode 4))

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
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key)

  ("C-h F" . helpful-function)
  ("C-c C-d" . helpful-at-point)
  :config
  (with-eval-after-load 'evil-maps
    (evil-define-key 'normal helpful-mode-map (kbd "q") 'quit-window)))

(use-package which-key
  :custom
  (which-key-show-early-on-C-h t)
  :config
  (which-key-mode 1))

(use-package popon
  :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon")
  :if (not (display-graphic-p)))

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

  (evil-set-leader '(normal visual operator motion) (kbd "SPC"))

  (evil-global-set-key 'motion (kbd "H") 'back-to-indentation)
  (evil-global-set-key 'motion (kbd "L") 'evil-end-of-line)

  (evil-global-set-key 'normal (kbd "Q") 'evil-window-delete))

(use-package better-jumper
  :config
  (better-jumper-mode 1)
  (with-eval-after-load 'evil-maps
    (evil-global-set-key 'motion (kbd "C-o") 'better-jumper-jump-backward)
    (evil-global-set-key 'motion (kbd "C-i") 'better-jumper-jump-forward)))

                                        ; Terminal Replacement
(use-package vterm
  :custom (vterm-always-compile-module t)
  :config
  (with-eval-after-load 'evil-maps
    (evil-define-key 'insert 'vterm-mode-map (kbd "C-u") 'vterm-send-C-u)))

                                        ; Editing Enhancements
(use-package evil-surround
  :hook ((prog-mode text-mode) . evil-surround-mode)
  :config
  (setq-default evil-surround-pairs-alist (push '(?< . ("< " . " >")) evil-surround-pairs-alist)
                evil-surround-pairs-alist (push '(?> . ("<" . ">")) evil-surround-pairs-alist)))

(use-package evil-nerd-commenter
  :config
  (with-eval-after-load 'evil-maps
    (evil-global-set-key 'normal (kbd "<leader>c") 'evilnc-comment-operator)
    (evil-global-set-key 'visual (kbd "<leader>c") 'evilnc-comment-or-uncomment-lines)))

;; autopairs
(use-package electric-pair
  :straight nil
  :no-require
  :config (electric-pair-mode 1))

                                        ; Movement Enhancements
(use-package evil-visualstar
  :config (global-evil-visualstar-mode 1))

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-keys '(?s ?l ?a ?g ?h ?v ?m ?e ?i ?r ?u ?w ?o ?c ?x ?d ?k ?f ?j))
  (avy-orders-alist '((avy-goto-char-2 . avy-order-closest)
                      (avy-goto-line . avy-order-closest)))
  :config
  (with-eval-after-load 'evil-maps
    (evil-define-key '(normal visual operator motion) 'global (kbd "s") 'evil-avy-goto-char-2)
    (evil-global-set-key 'motion (kbd "<leader>j") 'evil-avy-goto-line-below)
    (evil-global-set-key 'motion (kbd "<leader>k") 'evil-avy-goto-line-above)))

                                        ; Window Management
(use-package windmove
  :custom (windmove-create-window t)
  :commands (windmove-left windmove-down windmove-up windmove-right)
  :config
  (with-eval-after-load 'evil-maps
    (evil-global-set-key 'normal (kbd "S-<left>") 'windmove-left)
    (evil-global-set-key 'normal (kbd "S-<down>") 'windmove-down)
    (evil-global-set-key 'normal (kbd "S-<up>") 'windmove-up)
    (evil-global-set-key 'normal (kbd "S-<right>") 'windmove-right)))

                                        ; Version Control
(use-package diff-hl
  :config
  (global-diff-hl-mode 1)

  (with-eval-after-load 'evil-maps
    (evil-global-set-key 'motion (kbd "]c") 'diff-hl-next-hunk)
    (evil-global-set-key 'motion (kbd "[c") 'diff-hl-previous-hunk)
    (evil-global-set-key 'normal (kbd "<leader>hs") 'diff-hl-stage-current-hunk)
    (evil-global-set-key 'normal (kbd "<leader>hU") 'diff-hl-unstage-file)))

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
  ;; hide commands in M-x which do not work in the current mode.
  (read-extended-command-predicate #'command-completion-default-include-p)
  (enable-recursive-minibuffers t)
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt))

  :config
  (vertico-mode 1))

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-literal orderless-regexp orderless-prefixes orderless-flex))
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion flex)))))

(use-package marginalia
  :after vertico
  :config (marginalia-mode 1))

(use-package consult)

(use-package corfu
  :preface
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer 1)

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)

  :config
  (global-corfu-mode 1)

  (with-eval-after-load 'evil-maps
    (evil-define-key 'insert 'corfu-map (kbd "RET") 'newline)
    (evil-define-key 'insert 'corfu-map (kbd "M-S-SPC") 'corfu-insert-separator)))

(use-package corfu-terminal
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal")
  :requires (corfu popon)
  :if (not (display-graphic-p))
  :config (corfu-terminal-mode 1))

(use-package corfu-doc
  :requires corfu
  :hook (corfu-mode . corfu-doc-mode)
  :config
  (with-eval-after-load 'evil-maps
    (evil-define-key 'insert 'corfu-map (kbd "M-d") #'corfu-doc-toggle)
    (evil-define-key 'insert 'corfu-map (kbd "M-p") #'corfu-doc-scroll-up)
    (evil-define-key 'insert 'corfu-map (kbd "M-n") #'corfu-doc-scroll-down)))

(use-package corfu-doc-terminal
  :straight (corfu-doc-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal")
  :requires (corfu-doc popon)
  :if (not (display-graphic-p))
  :config (corfu-doc-terminal-mode 1))
