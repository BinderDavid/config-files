;;;; ---------------------------------------------------------------------------
;;;; Emacs Configuration
;;;; ---------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; General Emacs Configuration
;;; ----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Melpa Configuration
;; -----------------------------------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; -----------------------------------------------------------------------------
;; Color Theme
;; -----------------------------------------------------------------------------

(load-theme 'dichromacy t)

;; -----------------------------------------------------------------------------
;; Line Numbering
;; -----------------------------------------------------------------------------

(global-linum-mode t)
(setq line-number-mode nil)
(setq column-number-mode t)

;; -----------------------------------------------------------------------------
;; Window Decorations
;; -----------------------------------------------------------------------------

(tool-bar-mode -1)    ; Disable Toolbar
(scroll-bar-mode -1)  ; Disable Scrollbar

;; -----------------------------------------------------------------------------
;; Emacs startup
;; -----------------------------------------------------------------------------

(setq inhibit-startup-screen t)
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil)

;; -----------------------------------------------------------------------------
;; IDO Mode (Interactively Do Things
;; -----------------------------------------------------------------------------

(ido-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;; -----------------------------------------------------------------------------
;; Minibuffer Options
;; -----------------------------------------------------------------------------

(set 'echo-keystrokes -1) ; Echo keystrokes without 1s delay.

;; -----------------------------------------------------------------------------
;; Neotree Configuration (Browsing directories)
;; -----------------------------------------------------------------------------

(require 'neotree)
(global-set-key [f8] 'neotree-toggle) ; With <F8> toggle the file browser
(setq neo-theme 'ascii)               ; Set style of the tree to ascii signs

;; -----------------------------------------------------------------------------
;; Magit Configuration
;; -----------------------------------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)

;; -----------------------------------------------------------------------------
;; Resizing Frames
;; -----------------------------------------------------------------------------

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally) ; Control-shift left
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; -----------------------------------------------------------------------------
;; Font settings
;; -----------------------------------------------------------------------------

(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-9"))

;; -----------------------------------------------------------------------------
;; Flycheck Config
;; -----------------------------------------------------------------------------

(require 'flycheck)
(global-flycheck-mode)                     ; Global Flycheck
(setq flycheck-check-syntax-automatically
      '(mode-enabled idle-change save))    ; Rerun check on idle and save

;; -----------------------------------------------------------------------------
;; Config from emacs' Custom system
;; -----------------------------------------------------------------------------

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file))

;;; ----------------------------------------------------------------------------
;;; Configuration for specific languages
;;; ----------------------------------------------------------------------------

;; -----------------------------------------------------------------------------
;; Haskell
;; -----------------------------------------------------------------------------

;; Haskell Mode

(require 'haskell-mode)
(require 'haskell-interactive-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; Liquid Haskell

(add-to-list 'load-path (expand-file-name "liquid-tip.el/" user-emacs-directory))

;; Configure flycheck-liquidhs, if you haven't already
;(add-hook 'haskell-mode-hook
;          '(lambda () (flycheck-select-checker 'haskell-liquid)))

;(add-hook 'literate-haskell-mode-hook
;          '(lambda () (flycheck-select-checker 'haskell-liquid)))

;(require 'liquid-types)

;; Toggle minor mode on entering Haskell mode.
;(add-hook 'haskell-mode-hook
;          '(lambda () (liquid-types-mode)))
;(add-hook 'literate-haskell-mode-hook
;	  '(lambda () (liquid-types-mode)))


;;----------------------------------
;; OCaml Mode
;;----------------------------------
;; Tuareg
(load "~/ocaml/tuareg/tuareg-site-file")
(dolist
   (var (car (read-from-string
	      (shell-command-to-string "opam config env --sexp"))))
  ;; Merlin
  (setenv (car var) (cadr var)))
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
  (when (and opam-share (file-directory-p opam-share))                                                                                             
    ;; Register Merlin
    (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
    (autoload 'merlin-mode "merlin" nil t nil)
    ;; Automatically start it in OCaml buffers
    (add-hook 'tuareg-mode-hook 'merlin-mode t)
    (add-hook 'caml-mode-hook 'merlin-mode t)
    ;; Use opam switch to lookup ocamlmerlin binary
    (setq merlin-command 'opam)))

;; -----------------------------------------------------------------------------
;; Idris
;; -----------------------------------------------------------------------------

;; (require 'idris-mode)

;; -----------------------------------------------------------------------------
;; Lean
;; -----------------------------------------------------------------------------

;(defvar lean-mode-required-packages
;  '(company dash dash-functional flycheck f
;            fill-column-indicator s))
;(let ((need-to-refresh t))
;  (dolist (p lean-mode-required-packages)
;    (when (not (package-installed-p p))
;      (when need-to-refresh
;        (package-refresh-contents)
;        (setq need-to-refresh nil))
;      (package-install p))))
;; Set up lean-root path
;(setq lean-rootdir "~/Lean/lean2-master")  ;; <=== YOU NEED TO MODIFY THIS
;(setq-local lean-emacs-path
;            (concat (file-name-as-directory lean-rootdir)
;                    (file-name-as-directory "src")
;                    "emacs"))
;(add-to-list 'load-path (expand-file-name lean-emacs-path))
;(require 'lean-mode)

;; -----------------------------------------------------------------------------
;; Coq
;; -----------------------------------------------------------------------------

;; Proof General

(load "~/.emacs.d/lisp/PG/generic/proof-site")
(setq proof-splash-enable nil)
(setq proof-electric-terminator-enable nil)
(setq proof-next-command-insert-space nil)
(setq PA-one-command-per-line nil)

;; Company Coq

(add-hook 'coq-mode-hook #'company-coq-mode)
(add-hook 'coq-mode-hook
          (lambda ()
            (setq-local prettify-symbols-alist
                        '(
			  ("Qed." . ?■)
                          ("Defined." . ?□)
			  ))))



;; -----------------------------------------------------------------------------
;; Elisp
;; -----------------------------------------------------------------------------

;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)

;; -----------------------------------------------------------------------------
;; Cubical Type Theory (CTT)
;; -----------------------------------------------------------------------------

(load-file "~/GitRepos/cubicaltt/cubicaltt.el")
(autoload 'cubicaltt-mode "cubicaltt" "cubical editing mode" t)
(setq auto-mode-alist (append auto-mode-alist '(("\\.ctt$" . cubicaltt-mode))))

;; -----------------------------------------------------------------------------
;; Uroboro
;; -----------------------------------------------------------------------------

; (load "~/GitRepos/gacodt/CoqHaskell/uroboro.el")
; (require 'uroboro-mode)

