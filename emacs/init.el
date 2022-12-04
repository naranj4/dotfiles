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
(setq straight-check-for-modifications '(check-on-save find-when-checking))

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

                                        ; Mapping Enhancements
(use-package general
  :config
  (general-create-definer my/leader
    :keymaps 'override
    :prefix "SPC"))

                                        ; Vi-Emulation
(use-package goto-chg)

(use-package evil
  :hook (emacs-startup . evil-mode)

  :custom
  (evil-undo-system 'undo-redo)
  (evil-want-C-u-scroll t)
  (evil-want-Y-yank-to-eol t)
  (evil-disable-insert-state-bindings t)

  :general
  (:states 'motion
    "s-<left>" 'back-to-indentation
    "H" 'back-to-indentation
    "s-<right>" 'evil-end-of-line
    "L" 'evil-end-of-line
    "Q" 'evil-window-delete

    ;; SPC must not be bound to ensure that operator mappings work with my/leader
    "SPC" nil)

  ;; this keeps changing to evil-execute-last-recorded-macro
  (:states 'normal "Q" 'evil-window-delete)

  :init
  (setq evil-insert-state-cursor 'bar
        evil-motion-state-cursor 'hbar
        evil-operator-state-cursor 'hbar))

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
  :custom (recentf-max-saved-items 50)
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
  ;; ensure to auto-wrap text after 100 columns
  (fill-column 100))

;; cursorline
(use-package hl-line-mode
  :straight nil
  :hook (prog-mode text-mode fundamental-mode)
  :custom (hl-line-sticky-flag nil))

;; line numbers
(use-package display-line-numbers-mode
  :straight nil
  :hook (prog-mode text-mode fundamental-mode))

;; visual fill (adds margins)
(use-package visual-fill-column
  :defer t
  :custom
  (visual-fill-column-extra-text-width '(15 . 15))
  (visual-fill-column-center-text t)
  :hook (org-mode . visual-fill-column-mode))

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

(use-package all-the-icons
  :hook (emacs-startup . (lambda ())))

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
  :hook ((prog-mode text-mode fundamental-mode) . turn-on-evil-quickscope-always-mode)
  :custom-face
  (evil-quickscope-first-face ((t (:inherit 'font-lock-constant-face :underline t :bold t))))
  (evil-quickscope-second-face ((t (:inherit 'font-lock-type-face :underline t :bold t)))))

(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :config (evil-terminal-cursor-changer-activate))

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
  :defer 0.5
  :custom
  (which-key-show-early-on-C-h t)
  :config
  (which-key-mode 1))

(use-package popon
  :straight (popon :type git :repo "https://codeberg.org/akib/emacs-popon")
  :unless (display-graphic-p))

                                        ; Terminal Replacement
(use-package vterm
  :commands vterm
  :custom (vterm-always-compile-module t)
  :general
  (:keymaps 'vterm-mode-map :states 'insert
            "C-u" 'vterm-send-C-u
            "C-d" 'vterm-send-C-d)
  (:states 'normal "M-i" 'vterm))

                                        ; Editing Enhancements
(use-package evil-surround
  :hook ((prog-mode text-mode fundamental-mode) . evil-surround-mode)
  :config
  (setq-default evil-surround-pairs-alist (push '(?< . ("< " . " >")) evil-surround-pairs-alist)
                evil-surround-pairs-alist (push '(?> . ("<" . ">")) evil-surround-pairs-alist)))

(use-package evil-nerd-commenter
  :general
  (my/leader :states 'normal "c" 'evilnc-comment-operator)
  (my/leader :states 'visual "c" 'evilnc-comment-or-uncomment-lines))

;; autopairs
(use-package electric-pair
  :straight nil
  :no-require
  :hook (evil-insert-state-entry . electric-pair-mode))

                                        ; Navigation Enhancements
(use-package better-jumper
  :hook (emacs-startup . better-jumper-mode)
  :general
  (:states 'motion
           "C-o" 'better-jumper-jump-backward
           "C-i" 'better-jumper-jump-forward))

(use-package evil-visualstar
  :after evil
  :config (global-evil-visualstar-mode 1))

(use-package avy
  :custom
  (avy-all-windows nil)
  (avy-keys '(?s ?l ?a ?g ?h ?v ?m ?e ?i ?r ?u ?w ?o ?c ?x ?d ?k ?f ?j))
  (avy-orders-alist '((avy-goto-char-2 . avy-order-closest)
                      (avy-goto-line . avy-order-closest)))
  :general
  (my/leader :states 'motion
             "j" 'evil-avy-goto-line-below
             "k" 'evil-avy-goto-line-above)
  (:states '(normal visual operator motion) "s" 'evil-avy-goto-char-2))

(use-package projectile
  :general
  (my/leader :states 'normal
           "ff" 'projectile-find-file
           "fg" 'projectile-find-file-dwim

           "fp" 'project-switch-project

           "FF" 'projectile-find-file-in-directory)
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
  ((prog-mode text-mode vc-dir-mode fundamental-mode) . turn-on-diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  :general
  (:states 'motion
           "]c" 'diff-hl-next-hunk
           "[c" 'diff-hl-previous-hunk)
  (my/leader :states 'motion
           "hs" 'diff-hl-stage-current-hunk
           "hU" 'diff-hl-unstage-file
           "hr" 'diff-hl-revert-hunk))

                                        ; Completion Enhancements
(use-package vertico
  :hook (emacs-startup . vertico-mode)

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
  (minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)))

(use-package embark
  :general
  (:keymaps 'vertico-map "M-;" 'embark-act)
  (:states 'normal "M-." 'embark-dwim)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package orderless
  :after vertico

  :preface
  (defvar my/orderless-fuzzy-styles '(orderless-literal orderless-regexp orderless-prefixes orderless-flex)
    "Orderless styles to use normally when fuzzy matching is desired in the completion function.")
  (defvar my/orderless-literal-styles 'orderless-literal
    "Orderless styles to use when literal matching is desired.")

  (defun my/match-components-fuzzily ()
    "Components match fuzzily for the rest of this minibuffer session."
    (interactive)
    (setq-local orderless-matching-styles my/orderless-fuzzy-styles))

  (defun my/match-components-literally ()
    "Components match literally for the rest of this minibuffer session."
    (interactive)
    (setq-local orderless-matching-styles my/orderless-literal-styles))

  (general-define-key :keymaps 'vertico-map
                      "M-f" 'my/match-components-fuzzily
                      "C-f" 'my/match-components-literally)

  :custom
  (orderless-matching-styles my/orderless-fuzzy-styles)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion flex)))))

(use-package marginalia
  :after vertico
  :hook (vertico-mode . marginalia-mode)
  :config
  (add-to-list 'marginalia-command-categories '(projectile-find-file . file)))

(use-package consult
  :custom
  (consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator /\
 --smart-case --line-number --column --trim --no-heading .")

  :general
  (my/leader :states 'normal
           "fb" 'consult-buffer
           "/" 'consult-line

           "fsr" 'consult-ripgrep
           "fhs" 'consult-recent-file)

  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root))))

(use-package corfu
  :straight (corfu :files (:defaults "extensions/*"))
  :preface
  (defun my/corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'my/corfu-enable-in-minibuffer 1)

  :hook
  (evil-insert-state-entry . corfu-mode)
  ;; Quit Corfu after exiting insert mode
  (evil-insert-state-exit . corfu-quit)

  ;; Ensure that this lazy loads in minibuffer if vertico is not active
  :commands corfu-mode

  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 1)
  (corfu-scroll-margin 2)

  :general
  (:keymaps 'corfu-map
            "RET" nil
            "M-S-SPC" 'corfu-insert-separator)

  :config (global-corfu-mode 1))

(use-package corfu-terminal
  :straight (corfu-terminal :type git :repo "https://codeberg.org/akib/emacs-corfu-terminal")
  :after (corfu popon)
  :unless (display-graphic-p)
  :config (corfu-terminal-mode 1))

(use-package corfu-history
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-history-mode)
  :custom (corfu-history-length 25))

(use-package corfu-popupinfo
  :straight nil
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)
  :custom (corfu-popupinfo-delay '(0.5 . 0.5))
  :general
  (:keymaps 'corfu-map
            "M-d" 'corfu-popupinfo-toggle
            "M-p" 'corfu-popupinfo-scroll-down
            "M-n" 'corfu-popupinfo-scroll-up)
  :config
  (set-face-attribute 'corfu-popupinfo nil :height 1.0))

(use-package cape
  :general
  ("C-c p p" 'completion-at-point) ;; capf
  ("C-c p t" 'complete-tag)        ;; etags
  ("C-c p d" 'cape-dabbrev)        ;; or dabbrev-completion
  ("C-c p h" 'cape-history)
  ("C-c p f" 'cape-file)
  ("C-c p k" 'cape-keyword)
  ("C-c p s" 'cape-symbol)
  ("C-c p a" 'cape-abbrev)
  ("C-c p i" 'cape-ispell)
  ("C-c p l" 'cape-line)
  ("C-c p w" 'cape-dict)
  ("C-c p \\" 'cape-tex)
  ("C-c p _" 'cape-tex)
  ("C-c p ^" 'cape-tex)
  ("C-c p &" 'cape-sgml)
  ("C-c p r" 'cape-rfc1345)
  :init
  (add-to-list 'completion-at-point-functions #'cape-file))

                                        ; Org-Mode
(use-package org
  :preface
  (defun my/org-mode-setup ()
    (org-indent-mode 1)
    (auto-fill-mode 1)
    (visual-line-mode 1))

  :hook (org-mode . my/org-mode-setup)
  :custom
  (org-ellipsis " ▶")
  :init
  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers))

(use-package org-roam
  :preface
  (defun my/create-roam-template (key desc type &optional template &rest overrides)
    "Helper function to add sane defaults to a roam template"
    (cond ((not template)
           (list key desc type))
          (t
           ;; add sane defaults for :target and :unnarrowed if they aren't already defined.
           ;; NOTE: there's probably a better way to handle pushing defaults without needing to stack
           ;; plist-puts
           (append
            (list key desc type template)
            (plist-put
             (plist-put overrides :unnarrowed (cl-getf overrides :unnarrowed t))
             :target (cl-getf
                      overrides
                      :target '(file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n\n")))))))

  :custom
  (org-roam-directory (file-truename "~/org-roam/"))
  (org-roam-capture-templates
   `(,(my/create-roam-template "d" "Default" 'plain "%?")

     ("m" "Meetings")
     ,(my/create-roam-template
       "md" "Default" 'plain
       ;; evaluate the filename early
       `(file ,(file-truename (expand-file-name "templates/meetings/default.org" user-emacs-directory)))
       :target '(file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :meeting:\n\n"))

     ("P" "Projects")
     ,(my/create-roam-template
       "Pp" "Project" 'plain
       `(file ,(file-truename (expand-file-name "templates/projects/default.org" user-emacs-directory)))
       :target '(file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :project:\n\n"))
     ,(my/create-roam-template
       "PP" "Project Design" 'plain
       `(file ,(file-truename (expand-file-name "templates/projects/project-design.org" user-emacs-directory)))
       :target '(file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :project:\n\n"))
     ,(my/create-roam-template
       "Pf" "Feature" 'plain
       `(file ,(file-truename (expand-file-name "templates/projects/feature.org" user-emacs-directory)))
       :target '(file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :project:feature:\n\n"))
     ,(my/create-roam-template
       "PF" "Feature Design" 'plain
       `(file ,(file-truename (expand-file-name "templates/projects/feature-design.org" user-emacs-directory)))
       :target '(file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :project:feature:\n\n"))

     ("p" "People")
     ,(my/create-roam-template
       "pd" "Default" 'plain
       ;; evaluate the filename early
       `(file ,(file-truename (expand-file-name "templates/people/default.org" user-emacs-directory)))
       :target '(file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+filetags: :people:\n\n"))
     ))

  :general
  (:prefix "C-c n"
           "l" 'org-roam-buffer-toggle
           "f" 'org-roam-node-find
           "i" 'org-roam-node-insert
           "c" 'org-roam-capture)
  (my/leader :states 'normal :prefix "SPC o"
    "l" 'org-roam-buffer-toggle
    "f" 'org-roam-node-find
    "i" 'org-roam-node-insert
    "c" 'org-roam-capture)

  ;; The usual org-cycle mapping is override by the jump-list forward mapping
  (:keymaps 'org-mode-map :states 'normal "M-TAB" 'org-cycle)

  (:keymaps 'org-mode-map :states 'motion
            "{" 'org-backward-paragraph
            "}" 'org-forward-paragraph
            "(" 'org-backward-sentence
            ")" 'org-forward-sentence)

  :config
  ;; Ensure that the 'org-roam-directory exists
  (mkdir (symbol-value 'org-roam-directory) t)

  (org-roam-db-autosync-mode))
