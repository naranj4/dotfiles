(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5)) (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

; Install packages
(straight-use-package 'doom-themes)
(straight-use-package 'doom-modeline)
(straight-use-package 'all-the-icons)
(straight-use-package 'solaire-mode)

(straight-use-package 'helpful)
(straight-use-package 'which-key)
(straight-use-package 'no-littering)

(straight-use-package 'avy)

(straight-use-package 'evil)
(straight-use-package 'goto-chg)
(straight-use-package 'better-jumper)
(straight-use-package 'evil-visualstar)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-nerd-commenter)
(straight-use-package 'evil-quickscope)

(straight-use-package 'vertico)
(straight-use-package 'marginalia)
(straight-use-package 'corfu)
(straight-use-package 'corfu-doc)
(straight-use-package 'consult)
; terminal compatibility
(unless (display-graphic-p)
  (straight-use-package
   '(popon :type git :repo "https://codeberg.org/akib/emacs-popon"))
  (straight-use-package
   '(corfu-terminal
     :type git
     :repo "https://codeberg.org/akib/emacs-corfu-terminal"))
  (straight-use-package
   '(corfu-doc-terminal
     :type git
     :repo "https://codeberg.org/akib/emacs-corfu-doc-terminal")))

; No littering
(require 'no-littering)

; Move auto-save files and backups
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq backup-directory-alist
      `((".*" . ,(no-littering-expand-var-file-name "back-up/"))))
(setq lock-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "lock-files/") t)))

; Move custom variable configuration
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

; General settings
(setq lexical-binding t)

(blink-cursor-mode 0)
(setq cursor-type 'box)

(setq visible-bell nil
      ring-bell-function 'my/flash-mode-line)
(defun my/flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq windmove-create-window t)

(global-display-line-numbers-mode 1)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)
(electric-pair-mode 1)

(global-whitespace-mode 1)
(setq whitespace-style
      '(face trailing tabs space-after-tab space-before-tab))

(set-face-attribute 'default nil :family "MesloLGS Nerd Font Mono" :height 160)

(setq indent-tabs-mode nil
      tab-always-indent 'complete)

(setq completion-styles '(basic partial-completion flex))

(setq display-buffer-base-action
      `((display-buffer-reuse-mode-window
	 display-buffer-in-side-window
	 display-buffer-same-window)
	. ((mode . (help-mode helpful-mode apropos-mode))
	   (side . right)
	   (window-width . 0.5))))

; History
(setq history-length 25)
(recentf-mode 1)
(savehist-mode 1)

(add-to-list 'recentf-exclude no-littering-var-directory)
(add-to-list 'recentf-exclude no-littering-etc-directory)

; Theme
(setq doom-themes-enable-bold t
      doom-themes-enable-italic t)

(load-theme 'doom-one t)
(solaire-global-mode 1)

; Modeline
(require 'all-the-icons)
(require 'doom-modeline)

(setq doom-modeline-unicode-fallback t)

(doom-modeline-mode 1)

; Delete word instead of kill word
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

(global-set-key (kbd "<M-backspace>") 'my/backward-delete-word)
(global-set-key (kbd "M-d") 'my/delete-word)

; Evil mode
(setq evil-insert-state-cursor 'bar
      evil-motion-state-cursor 'hbar
      evil-operator-state-cursor 'hbar)

(setq evil-undo-system 'undo-redo
      evil-want-C-u-scroll t
      evil-want-Y-yank-to-eol t
      evil-disable-insert-state-bindings t)

(require 'evil)
(evil-mode 1)
(evil-set-leader '(normal visual operator motion) (kbd "SPC"))

(evil-define-key '(normal visual operator motion) 'global (kbd "H") 'back-to-indentation)
(evil-define-key '(normal visual operator motion) 'global (kbd "L") 'evil-end-of-line)

(evil-define-key '(normal) 'global (kbd "Q") 'evil-window-delete)

; jump list
(require 'better-jumper)
(better-jumper-mode 1)
(with-eval-after-load 'evil-maps
  (define-key evil-motion-state-map (kbd "C-o") 'better-jumper-jump-backward)
  (define-key evil-motion-state-map (kbd "C-i") 'better-jumper-jump-forward))

; quick window switching
(evil-define-key '(normal) 'global (kbd "S-<left>") 'windmove-left)
(evil-define-key '(normal) 'global (kbd "S-<down>") 'windmove-down)
(evil-define-key '(normal) 'global (kbd "S-<up>") 'windmove-up)
(evil-define-key '(normal) 'global (kbd "S-<right>") 'windmove-right)

; surround
(global-evil-surround-mode 1)
(setq-default evil-surround-pairs-alist (push '(?< . ("< " . " >")) evil-surround-pairs-alist)
	      evil-surround-pairs-alist (push '(?> . ("<" . ">")) evil-surround-pairs-alist))

; commenter
(require 'evil-nerd-commenter)
(evil-define-key '(normal) 'global (kbd "<leader>c") 'evilnc-comment-operator)
(evil-define-key '(visual) 'global (kbd "<leader>c") 'evilnc-comment-or-uncomment-lines)

; quickscope
(require 'evil-quickscope)
(global-evil-quickscope-always-mode 1)
(set-face-attribute 'evil-quickscope-first-face nil :underline t :bold t)
(set-face-attribute 'evil-quickscope-second-face nil :inherit 'font-lock-type-face :underline t :bold t)

(global-evil-visualstar-mode 1)

; Avy
(setq avy-all-windows nil
      avy-keys '(?s ?l ?a ?g ?h ?v ?m ?e ?i ?r ?u ?w ?o ?c ?x ?d ?k ?f ?j)
      avy-orders-alist '((avy-goto-char-2 . avy-order-closest)
			 (avy-goto-line . avy-order-closest)))

(evil-define-key '(normal visual operator motion) 'global (kbd "s") 'evil-avy-goto-char-2)
(evil-define-key '(normal visual operator motion) 'global (kbd "<leader>j") 'evil-avy-goto-line-below)
(evil-define-key '(normal visual operator motion) 'global (kbd "<leader>k") 'evil-avy-goto-line-above)

; Helpful
(global-set-key (kbd "C-h f") #'helpful-callable)

(global-set-key (kbd "C-h v") #'helpful-variable)
(global-set-key (kbd "C-h k") #'helpful-key)

;; Lookup the current symbol at point. C-c C-d is a common keybinding
;; for this in lisp modes.
(global-set-key (kbd "C-c C-d") #'helpful-at-point)

;; Look up *F*unctions (excludes macros).
;;
;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
;; already links to the manual, if a function is referenced there.
(global-set-key (kbd "C-h F") #'helpful-function)

;; Look up *C*ommands.
;;
;; By default, C-h C is bound to describe `describe-coding-system'. I
;; don't find this very useful, but it's frequently useful to only
;; look at interactive functions.
(global-set-key (kbd "C-h C") #'helpful-command)

(evil-define-key '(normal) helpful-mode-map (kbd "q") 'quit-window)

; which-key
(require 'which-key)

(setq which-key-show-early-on-C-h t)

(which-key-mode 1)

; Vertico
(require 'vertico)
(vertico-mode 1)
(setq vertico-cycle t)

(defun my/crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'my/crm-indicator)

;; Do not allow the cursor in the minibuffer prompt
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Emacs 28: Hide commands in M-x which do not work in the current mode.
;; Vertico commands are hidden in normal buffers.
(setq read-extended-command-predicate
      #'command-completion-default-include-p)

;; Enable recursive minibuffers
(setq enable-recursive-minibuffers t)

; Marginalia
(marginalia-mode 1)

; Corfu
(setq corfu-cycle t
      corfu-auto t
      corfu-auto-delay 0
      corfu-auto-prefix 2)
(global-corfu-mode 1)

(defun corfu-enable-always-in-minibuffer ()
  "Enable Corfu in the minibuffer if Vertico/Mct are not active."
  (unless (or (bound-and-true-p mct--active)
              (bound-and-true-p vertico--input))
    (corfu-mode 1)))
(add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

(add-hook 'corfu-mode-hook #'corfu-doc-mode)

(unless (display-graphic-p)
  (corfu-terminal-mode 1)
  (corfu-doc-terminal-mode 1))

(define-key corfu-map (kbd "M-p") #'corfu-doc-scroll-down)
(define-key corfu-map (kbd "M-n") #'corfu-doc-scroll-up)
(define-key corfu-map (kbd "M-d") #'corfu-doc-toggle)
