;; Packages will be initialized by use-package later.
(setq package-enable-at-startup nil)

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

(add-hook 'emacs-startup-hook
      (lambda () (setq gc-cons-threshold (* 50 1000 1000))))

;; Do not resize the frame at this early stage.
(setq frame-inhibit-implied-resize t)

;; Prevent unwanted runtime builds; packages are compiled ahead-of-time when
;; they are installed and site files are compiled when gccemacs is installed.
(setq comp-deferred-compilation nil
     native-comp-deferred-compilation nil)

;; Don't show startup message when Emacs starts
(setq inhibit-startup-message t)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements
(push '(font . "MesloLGS Nerd Font Mono-16") default-frame-alist)
(push '(width . 100) default-frame-alist)
(push '(height . 50) default-frame-alist)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

(push '(cursor-type . box) default-frame-alist)

(push '(left-fringe . 4) default-frame-alist)
(push '(right-fringe . 4) default-frame-alist)
